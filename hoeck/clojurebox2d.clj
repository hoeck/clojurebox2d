
(ns hoeck.clojurebox2d
  (:use hoeck.clojurebox2d.processing
        hoeck.thread
        hoeck.iterate
        rosado.processing
        (clojure.contrib pprint except def))
  (:require [hoeck.clojurebox2d.jbox2d :as jbox])
  (:import (processing.core PApplet)

           (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter
                           ComponentListener)
           (javax.swing JFrame JLabel JTextField JButton)
           
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue)
           
           (org.jbox2d.collision Shape PolygonShape CircleShape)
           (org.jbox2d.dynamics Body World)))

;; clojurebox environment

(def cbox {:world-jobs nil;; atom map of [name fn], called each step with the current step number
           :frame nil;; the JFrame containing the PApplet
           :applet nil;; PApplet processing environment
           })

(defmacro with-applet
  "Evaluate body with *applet* bound to the current PApplet
  (the processing environment, (:applet cbox)"
  [& body]
  `(binding [*applet* (:applet cbox)]
     ~@body))

;; default window events

(declare redef)
(defn window-closing
  "Called when the clojurebox frame is about to be closed."
  []
  ;; when closing the frame
  ;; stop the physhics sim and processing
  (println "WINDOW CLOSING!!!")
  (interrupt 'jbox-physics)
  (redef draw (.stop *applet*)))

(defn window-resized
  "Called when the clojurebox frame is resized."
  [])

(declare add-job add-task run-world-tasks)
(defn init
  "initialize clojurebox2d.
  Start processing and create dedicated jbox2d thread."
  ([] (init {}))
  ([opts-map]
     (let [physics-frames 60.0
           render-frames 30.0
           ;; creates its own render thread, returns the frame and applet:
           [frm app] (setup-processing :size [320 200])
           wjobs (jbox/start-world-thread
                  #(jbox/create-world opts-map)
                  physics-frames)]
       (.addWindowListener frm (proxy [WindowAdapter] []
                                 (windowClosing [e] (with-applet (window-closing)))))
       (.addComponentListener frm (proxy [ComponentListener] []
                                    (componentResized [e] (with-applet (window-resized)))
                                    (componentMoved [e])
                                    (componentHidden [e])
                                    (componentShown [e])))
       (alter-var-root #'cbox merge
                       {:world-jobs wjobs
                        :frame frm
                        :applet app})
       ;; post-init

       ;; makes the with-world thingy work
       (add-job 'run-tasks run-world-tasks)))) 
    

;; job control

(defn add-job [name job]
  (swap! (:world-jobs cbox) assoc
         name job))

(defn remove-job [name]
  (swap! (:world-jobs cbox) dissoc name))

(defn remove-all-jobs []
  (swap! (:world-jobs cbox) (constantly {})))

(defn list-jobs []
  (keys @(:world-jobs cbox)))

;; jbox objects

(defn query
  "Return an array of shapes which potentially overlap with the given aabb.
  aabb defaults to the world aabb."
  ([#^World world] 
     (query world (.getWorldAABB world)))
  ([world v0 v1]
     (query world (jbox/make-aabb v0 v1)))
  ([world x0 y0 x1 y1]
     (query world (jbox/make-aabb x0 y0 x1 y1)))
  ([#^World world aabb]
     (.query world aabb (.getBodyCount world))))

(defn get-shapes ;; OBSOLETE
  "Return an array of jbox2d Shapes."
  ([#^World world]
     (get-shapes world (.getWorldAABB world)))
  ([world v0 v1]
     (get-shapes world (jbox/make-aabb v0 v1)))
  ([world x0 y0 x1 y1]
     (get-shapes world (jbox/make-aabb x0 y0 x1 y1)))  
  ([#^World world aabb]
     (.query world aabb (.getBodyCount world))))

(defn get-shape-points
  "given a polygon shape, return a seq of its shape corners
  in world-coordinates."
  [#^PolygonShape shape] ;; for polygon shapes only!
  (iter (let #^Body body (.getBody shape))
        (for vert in-array (.getVertices shape))
        (collect (jbox/vec2->clj (.getWorldPoint body vert)))))

(defn get-circle-points
  "Given a circle shape, return its center in world coordinates and
  its radius: [center, radius]"
  [#^CircleShape shape]
  (vector (jbox/vec2->clj (.getWorldPoint (.getBody shape) (.getLocalPosition shape)))
          (.getRadius shape)))

(defn clear-world
  "remove all objects from world"
  [#^World world]
  (iter (for b call .getNext on (.getBodyList world))
        (do (.destroyBody world b)
            (recur))))

;; body-userdata

(defstruct body-userdata-struct :name :draw-fn :box :circle)

(defmacro body-userdata
  "Expand to code which calls the function f with the 
  bodys userdata (a structmap) and any given additional args.
  If no function is given, just return the bodys userdata."
  ([body] `(.getUserData ~body))
  ([body f] `(-> ~body .getUserData ~f))
  ([body f & args] `(-> ~body .getUserData (~f ~@args))))

(defmacro make-body-userdata [& keyvals]
  `(struct-map body-userdata-struct ~@keyvals))

(defmacro alter-body-userdata [body f & args]
  `(.setUserData ~body (body-userdata ~body ~f ~@args)))

;; world step job

(def step-ptime (atom (/ 1 60)));; slow-motion, 60 is normal

(defn step-world [#^World world tick state]
  ;; ptime, iterations
  (let [t @step-ptime]
    (if (< 0 t)
      (.step world t 10))))
  
;; world task job

(def world-tasks (ConcurrentLinkedQueue.))

(defn add-task [task-fn] (.add world-tasks task-fn))

(defn run-world-tasks
  "A job to run one-time tasks on world, e.g. adding bodies.
  Use the with-world macro to safely execute tasks within the
  world thread."
  [world tick state]
  (doseq [t (take-while identity (repeatedly #(.poll #^ConcurrentLinkedQueue world-tasks)))]
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
                (let [task-result (try (f world tick state)
                                       ;; don't interrupt the world thread
                                       (catch Exception e e))]
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

;; set hooks into the various places of clojurebox
(def cljbox-hook-vars {'window-resized `window-resized,
                       'window-closing `window-closing})

(defn redef*
  {:doc (str "Use f as the new definition of the function name." \newline
             "  Redefinable functions include:" \newline
             (apply str (map #(str "    " % \newline) (keys processing-methods))) \newline
             "  for processing and " \newline
             (apply str (map #(str "    " % \newline) (keys cljbox-hook-vars))) \newline
             "  for clojurebox2d hooks.")}
  [hook-name f]
  (if-let [full-name (or (processing-methods hook-name) (cljbox-hook-vars hook-name))]
    (do (println "redefining:" full-name)
        (alter-var-root (resolve full-name) (constantly f)))
    (throwf "Unknown place: %s" hook-name)))



(defmacro redef
  "Redefine a (usually) argless hook-function. See (doc redef*) for
  possible functions. Processing functions are usually executed within the
  processing thread and a bound *applet*."
  [name & body]
  `(redef* '~name (fn [] ~@body)))

(defalias make-body jbox/make-body)
(defalias make-aabb jbox/make-aabb)
(defalias vec2 jbox/vec2)
(defalias vec2->clj jbox/vec2->clj)
(defalias make-mass jbox/make-mass)

(println "clojurebox2d")



