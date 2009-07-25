
(ns hoeck.clojurebox2d
  (:use hoeck.clojurebox2d.processing
        hoeck.thread
        rosado.processing)
  (:require [hoeck.clojurebox2d.jbox2d :as jbox])
  (:import (processing.core PApplet)

           (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter)
           (javax.swing JFrame JLabel JTextField JButton)
           
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue)))

;; clojurebox environment
(def cbox {:world-add-task nil;; function wich places given a function on the world-threads one-time queue
           :world-jobs nil;; atom map of [name fn], called each step with the current step number
           :frame nil;; the JFrame containing the PApplet
           :applet nil;; PApplet processing environment
           :camera (atom [[0 0] 1]);; [offset from center, scale], use set-pos and set-zoom
           })


(defmacro with-world ;; todo: split that into a driver-fn and a macro
  "execute body in the world thread, wait for its result and return it.
Waits no more than 2 seconds for the result and throws an Exception on timeout.
If body throws an exception, it is re-thrown in the current thread.
Establishes a binding to world and tick."
  [& body]
  `(let [ret# (java.util.concurrent.ArrayBlockingQueue. 1)]
     ((:world-add-task cbox)
      (fn [~'world ~'tick]
        (let [result# (try ~@body (catch Exception e# e#))]
          (if (isa? (class result#) Exception)
            (.add ret# result#)
            (.add ret# [result#])))));; wrap result in vector, to be able to return nil
     (if-let [return-val# (.poll ret# 2 (timeunit :sec))]
       (if (vector? return-val#)
         (first return-val#)
         (throw return-val#))
       (throw (Exception. "with-world timed out.")))))

(defmacro with-applet
  "Evaluate body with *applet* bound to the current PApplet
  (the processing environment, (:applet cbox)"
  [& body]
  `(binding [*applet* (:applet cbox)]
     ~@body))

(defn init
  "initialize clojurebox2d.
  Start processing and create dedicated jbox2d thread."
  []
  (let [physics-frames 60.0
        render-frames 25.0
        app (make-applet)
        ;; creates its own render thread, returns the applet:
        frm (setup-processing app :size [320 200])
        wprop (jbox/start-world-thread
               #(jbox/create-world)
               physics-frames)]
    (.addWindowListener frm (proxy [WindowAdapter] []
                              (windowClosed [e] ;; stop the physhics sim when closing the frame
                                            ((:world-add-task wprop) (fn [& _] (interrupt))))))
    (dosync (alter-var-root #'cbox merge
                            wprop
                            {:frame frm
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

;; camera control

(defn set-zoom [zoom]
  (swap! (:camera cbox) assoc 1 zoom))

(defn set-pos [x y]
  (swap! (:camera cbox) assoc 0 [x y]))

;; world-state-writer job
(def world-bodies (atom []))

(defn write-world-state [world tick]
  ;; every 2 ticks
  (if (even? tick)
    (let [clojure-body-definitions (query-world world)]
      (swap! world-bodies (constantly clojure-body-definitions)))))

;; world step job
(def step-ptime (/ 1 120)) ;; slow-motion, 60 is normal

(defn step-world [world tick]
  ;; ptime, iterations
  (.step world step-ptime 10))
  

(defn example []

  ;; test run
  (init)

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
                     :dynamic true}))
  
  ;; setting camera
  (set-zoom 7)
  (set-pos 150 150)
  
  ;; adding the draw function  
  (alter-var-root #'draw
                  (constantly
                   (fn []
                     (stroke-float 0)
                     (fill 255)
                     (background-int 255)
                     (wire-draw (map second @world-bodies)))))
  
  ;; add job: observing jbox objects
  (add-job 'write-state write-world-state)

  ;; add job: stepping the jbox engine
  ;; this actually starts the simulation
  (add-job 'step step-world)
  
  ;; watch :)
  
  ;; finally, remove all objects form world
  (comment (with-world (clear-world world))))


(println "clojurebox loaded")