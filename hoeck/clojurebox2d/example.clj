
(ns hoeck.clojurebox2d.example
  (:use hoeck.clojurebox2d
        hoeck.iterate
        rosado.processing
        clojure.contrib.pprint))

;; camera

(def my-camera (atom [[0 0] 1])) ;; [offset, scale]

(defn set-my-camera-zoom [zoom]
  (swap! my-camera assoc 1 zoom))

(defn set-my-camera-pos [x y]
  (swap! my-camera assoc 0 [x y]))

;; world-state

(defn jbox->clojurebox
  "Function to translate jbox objects into clojure datastructures.
  Return [name, [points*-clockwise] selected-p] from a body"
  [shape]
  (let [body (.getBody shape)]
    (vector (get-shape-points shape)
            (body-userdata body :name)
            (body-userdata body :draw-fn))))

(def world-state (atom []))

(defn write-world-state [world tick state]
  (if (even? tick) ;; every 2 ticks
    (let [clojure-body-definitions (iter (for shape in-array (query world))
                                         (collect (jbox->clojurebox shape)))]
      (swap! world-state (constantly clojure-body-definitions)))))

(comment (with-world (iter (for shape in-array (query world))
                           (collect (jbox->clojurebox shape)))))

;;   comparison: iter vs. plain loop
;;
;;(iter (for shape call .next on (query world)) (collect (jbox->clojurebox shape)))
;;(loop [shape (query world) bodies []] (if shape (recur (.next shape) (conj bodies (jbox->clojurebox shape))) bodies))
;;
;; iter: 81 chars (70%)
;;       2 levels of nesting
;;       1 temporary binding: shape
;; loop: 117 chars (144%)
;;       2 levels of nesting
;;       2 tmp bindings: shape, bodies

(defn draw-background []
  (background-int 255))

(defn default-style []
  (stroke-float 0)
  (fill 255))

(defn box-draw [[v1 v2 v3 v4]]
  (quad (.x v1) (.y v1)
        (.x v2) (.y v2)
        (.x v3) (.y v3)
        (.x v4) (.y v4)))

(defn selected-box-draw [shape-points]
  (fill 0)
  (box-draw shape-points))

(defn my-draw
  "Draws all body shapes (only boxes for now) using quad."
  [world-state]
  (let [[offset, zoom] @my-camera]
    (translate offset)
    (scale zoom)
    (draw-background)
    (default-style)
    (doseq [[shape-points name draw-fn] world-state]
      (if draw-fn
        (draw-fn shape-points);; assume boxes
        (do (default-style)
            (box-draw shape-points))))))

;; center the current world on resize
(redef window-resized
  (let [dimension (.getSize (:frame cbox))
        width (. dimension width)
        height (. dimension height)]
    (set-my-camera-pos (int (/ width 2))
                       (int (/ height 2)))))

;; bodies

(defn bodies-info []
  (with-world (map #(body-userdata %) (query-world-bodies world))))

;; test run

(defn example []

  (init)

  ;; add some test objects
  (with-world 
    (make-body world 'ground
               {:shape [10 1]
                :pos [0 0]
                :dynamic false}))

  (with-world
    (make-body world 'box1
               {:shape [1 1]
                :pos [0 -10]
                :dynamic true})
    (make-body world 'box2
               {:shape [0.5 0.5]
                :pos [0.8 -5]
                :dynamic true})
    (make-body world 'box3
               {:shape [1 0.5]
                :pos [0.2 -2]
                :dynamic true}))

  ;; setting camera
  (set-my-camera-zoom 13)
  ;;(set-pos 150 150) -> set by window-resized
  
  ;; adding the draw function  
  (redef draw
         (my-draw @world-state))

  ;; add job: observing jbox objects
  (add-job 'write-state write-world-state)

  ;; add job: stepping the jbox engine
  ;; this actually starts the simulation
  (add-job 'step step-world)
  (comment (remove-job 'step))
  ;; watch :)
  
  ;; finally, remove all objects form world
  (comment (with-world (clear-world world))))

(defn mouse-coords
  "given *applet* and and optionally pixel-coordinates,
  calculate the current position in world-coordinates."
  ([] (mouse-coords (mouse-x) (mouse-y)))
  ([mx my]
     (let [;; camera
           [[cx cy] zoom] @my-camera
           ;; -> world
           wx (/ (- mx cx) (float zoom))
           wy (/ (- my cy) (float zoom))]
       [wx wy])))

;; select a body, and store its name in the worldthreads state
(defn mouse-select-body
  "use the current mouse pointer to select a body. Return the body's
  name or nil if no body is under the mouse-ptr.
  Additionally, store a reference at :selected-body in the world-threads
  state.
  Optionally call select-hook with the newly selected body (will be nil if
  no body was selected) and (old) state."
  [& select-hook]
  (let [[wx wy] (mouse-coords)]
    (println "pressed" wx wy)
    (with-world
      ;; always select the first body
      (let [b (iter (let w (vec2 wx wy))
                    (let d (vec2 0.1 0.1))
                    (for shape in-array (query world (.sub w d) (.add w d)))
                    (for body as (.getBody shape))
                    (return-if (and (not (.isStatic body))
                                    (.testPoint shape (.getXForm body) w))
                               body))]
        (if-let [h (first select-hook)] (h b state))
        (if b (body-userdata b :name))))))


(defn highlight-and-store-one-selected-body [body state]
  (if-let [old-body (.get state :selected-body)]
    (alter-body-userdata old-body assoc :draw-fn nil))
  (when body
    (alter-body-userdata body assoc :draw-fn selected-box-draw))
  (.put state :selected-body body))

;; on click, select a body, and show that its selected, and drag it around
(redef mouse-pressed
  (mouse-select-body (fn [body state]                       
                       (try
                        (when body
                          (println "putting body to sleep:" body)
                          ;;(.putToSleep body)
                          (.setMass body (make-mass 0 0)) ;; mmmh ..., makes it static, mentioned somewhere in the docs, but i would prefer a .setStatic method
                          )
                        (catch Exception e (println "Exception:" e)))
                       (highlight-and-store-one-selected-body body state))))

(redef mouse-released
    (with-world
      (when-let [b (.get state :selected-body)]
        (highlight-and-store-one-selected-body nil state)
        (println "waking up body:" b "NOOOO")
        (try
         ;;(.wakeUp b)
         (.setMassFromShapes b)
         (catch Exception e (println "Exception:" e))))))

(redef mouse-dragged
  (let [[x y] (mouse-coords)]
    (with-world 
      (when-let [b (.get state :selected-body)]
        (println :body-moved x y)
        (.setXForm b
                   (hoeck.clojurebox2d.jbox2d/vec2 x y)
                   (-> b .getAngle))))))

