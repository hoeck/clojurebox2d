
(ns hoeck.clojurebox2d
  (:use hoeck.clojurebox2d.processing
        hoeck.thread
        rosado.processing
        clojure.contrib.pprint
        clojure.contrib.except)
  (:require [hoeck.clojurebox2d.jbox2d :as jbox])
  (:import (processing.core PApplet)

           (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter
                           ComponentListener)
           (javax.swing JFrame JLabel JTextField JButton)
           
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue)))

;; clojurebox environment
(def cbox {:world-jobs nil;; atom map of [name fn], called each step with the current step number
           :frame nil;; the JFrame containing the PApplet
           :applet nil;; PApplet processing environment
           :camera (atom [[0 0] 1]);; [offset from center, scale], use set-pos and set-zoom
           })

;; camera control

(defn set-zoom [zoom]
  (swap! (:camera cbox) assoc 1 zoom))

(defn set-pos [x y]
  (swap! (:camera cbox) assoc 0 [x y]))

;; processing hooks

(defn set-processing
  "Set the processing.core.PApplet method name to f.
  F is always called with the current applet as the only argument.
  Preferably use (processing-fn ..) instead of `fn' to create those functions.
  See `processing-methods' for valid names, e.g.:
    `draw' .. called each frame, to draw sth on the screen
    `key-(pressed, relased, typed)' .. called on key events
    `mouse-(clicked, pressed, dragged, released)' .. called on mouse events" 
  [name f]
  (let [method-name (processing-methods name)
        applet (:applet cbox)]
    (if applet
      (.__updateClojureFnMappings applet {method-name f})
      (throwf "Processing not initialized! (:applet cbox) is nil"))))

(defmacro processing-fn
  "creates a function which takes an argument (the applet) and binds that
  to *applet* before executing body.
  (the rosado.processing functions need *applet*)"
  [& body]
  `(fn [applet#] 
     (binding [*applet* applet#]
       ~@body)))

(defmacro with-applet
  "Evaluate body with *applet* bound to the current PApplet
  (the processing environment, (:applet cbox)"
  [& body]
  `(binding [*applet* (:applet cbox)]
     ~@body))

;; default window events

(defn window-closing
  "Called when the clojurebox frame is about to be closed."
  []
  ;; when closing the frame
  ;; stop the physhics sim and processing
  (println "WINDOW CLOSING!!!")
  (interrupt 'jbox-physics)
  (set-processing 'draw (processing-fn (.stop *applet*))))

(defn window-resized
  "Called when the clojurebox frame is resized."
  []
  (let [dimension (.getSize (:frame cbox))
        width (. dimension width)
        height (. dimension height)]
    (set-pos (int (/ width 2))
             (int (/ height 2)))))

(defn init
  "initialize clojurebox2d.
  Start processing and create dedicated jbox2d thread."
  []
  (let [physics-frames 60.0
        render-frames 25.0
        ;; creates its own render thread, returns the frame and applet:
        [frm app] (setup-processing :size [320 200])
        wjobs (jbox/start-world-thread
               #(jbox/create-world)
               physics-frames)]
    (.addWindowListener frm (proxy [WindowAdapter] []
                              (windowClosing [e] (window-closing))))
    (.addComponentListener frm (proxy [ComponentListener] []
                                 (componentResized [e] (window-resized))
                                 (componentMoved [e])
                                 (componentHidden [e])
                                 (componentShown [e])))
    (dosync (alter-var-root #'cbox merge
                            {:world-jobs wjobs
                             :frame frm
                             :applet app}))))

;; job control

(defn add-job [name job]
  (swap! (:world-jobs cbox) assoc
         name job))

(defn remove-job [name]
  (swap! (:world-jobs cbox) dissoc name))

(defn list-jobs []
  (keys @(:world-jobs cbox)))

;; objects

(defn get-shapes
  "Return an array of jbox2d Shapes."
  ([world]
     (get-shapes world (.getWorldAABB world)))
  ([world aabb]
     (.query world aabb (.getBodyCount world))))

(defn get-shape-points
  "given a shape, return a seq of its shape corners 
  in world-coordinates."
  [shape] ;; for simple shapes
  (let [verts (.getVertices shape) ;;(.getShapeList shape)
        body (.getBody shape)]
    (map #(.getWorldPoint body %) verts)))

(defn clear-world
  "remove all objects from world"
  [world]
  (doseq [b (map #(.getBody %) (get-shapes world))]
    (.destroyBody world b)))

;; jbox objects -> clojure datastructures
(defn query-world
  "Return a seq of vectors: [body-name, [shape-points*-clockwise]]."
  [world]
  (doall
   (map 
    ;; [name, [points*-clockwise]]
    #(vector 
      (-> % .getBody .getUserData)
      (get-shape-points %))
    (get-shapes world))))

;; processing

(defn wire-draw
  "Draws all body shapes (only boxes for now) using quad."
  [points-vec]
  (let [[offset, zoom] @(:camera cbox)]
    (translate offset)
    (scale zoom)
    (doseq [[v1 v2 v3 v4] points-vec] ;; assume boxes      
      (quad (.x v1) (.y v1)
            (.x v2) (.y v2)
            (.x v3) (.y v3)
            (.x v4) (.y v4)))))


;; world-state-writer job

(def world-bodies (atom []))

(defn write-world-state [world tick state]
  ;; every 2 ticks
  (if (even? tick)
    (let [clojure-body-definitions (query-world world)]
      (swap! world-bodies (constantly clojure-body-definitions)))))

;; world step job

(def step-ptime (/ 1 60)) ;; slow-motion, 60 is normal

(defn step-world [world tick state]
  ;; ptime, iterations
  (.step world step-ptime 10))
  
;; world task job

(def world-tasks (ConcurrentLinkedQueue.))

(defn add-task [task-fn] (.add world-tasks task-fn))

(defn run-world-tasks
  "A job to run one-time tasks on world, e.g. adding bodies."
  [world tick state]
  (doseq [t (take-while identity (repeatedly #(.poll world-tasks)))]
    (t world tick state)))

(defn with-world*
  "execute f in the world thread, wait for its result and return it.
  f is called with three args: world, tick and state.
  Waits no more than 2 seconds for the result and throws an Exception on timeout.
  If f throws an exception, it gets catched in the worlds thread and is 
  re-thrown in the current thread."
  [f]
  (let [result-queue (java.util.concurrent.ArrayBlockingQueue. 1)]
    (add-task (fn [world tick state]
                (let [task-result (try (f world tick state) (catch Exception e e))] ;; don't interrupt the world thread
                  (if (isa? (class task-result) Exception)
                    (.add result-queue task-result)
                    (.add result-queue [task-result]))))) ;; wrap result in vector, to be able to return nil
    (if-let [ret (.poll result-queue 2 (timeunit :sec))]
      (if (vector? ret)
        (first ret)
        (throw ret))
      (throw (Exception. "with-world* timed out.")))))

(defmacro with-world
  "Execute body within the worlds thread.
  Binds 'world', 'tick' and 'state'. See the documentation of
  with-world* for more info."
  [& body]
  `(with-world* (fn [~'world ~'tick ~'state] ~@body)))


(defn example []

  ;; test run
  (init)
  (add-job 'run-tasks run-world-tasks)

  ;; add some test objects
  (with-world 
    (jbox/make-body world 'ground
                    {:shape [10 1]
                     :pos [0 0]
                     :dynamic false}))

  (with-world
    (jbox/make-body world 'box1
                    {:shape [1 1]
                     :pos [0 -10]
                     :dynamic true})
    (jbox/make-body world 'box2
                    {:shape [0.5 0.5]
                     :pos [0.8 -5]
                     :dynamic true})
    (jbox/make-body world 'box3
                    {:shape [1 0.5]
                     :pos [0.2 -2]
                     :dynamic true}))
  
  ;; setting camera
  (set-zoom 7)
  ;;(set-pos 150 150) -> set by window-resized
  
  ;; adding the draw function  
  (set-processing 'draw
                  (processing-fn
                    (stroke-float 0)
                    (fill 255)
                    (background-int 255)
                    (wire-draw (map second @world-bodies))))
  
  ;; add job: observing jbox objects
  (add-job 'write-state write-world-state)

  ;; add job: stepping the jbox engine
  ;; this actually starts the simulation
  (add-job 'step step-world)
  
  ;; watch :)
  
  ;; finally, remove all objects form world
  (comment (with-world (clear-world world))))


(println "clojurebox2d")
