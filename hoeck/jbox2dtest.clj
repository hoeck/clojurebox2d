

(ns hoeck.jbox2dtest
  (:use rosado.processing
        (clojure.contrib pprint def)
        hoeck.library
        hoeck.thread)
  (:import (org.jbox2d.common Color3f Settings Vec2)
           (org.jbox2d.collision PolygonDef CircleDef ContactID Shape AABB)
           (org.jbox2d.dynamics Body BodyDef BoundaryListener ContactListener
                                DebugDraw DestructionListener World)
           (org.jbox2d.dynamics.contacts ContactResult)
           (org.jbox2d.dynamics.joints Joint MouseJoint MouseJointDef)
           
           (processing.core PApplet)

           (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter)           
           (javax.swing JFrame JLabel JTextField JButton)))

(comment
  (defn unsupported-operation!
    "throw a java.lang.UnsupportedOperationException with messages as text."
    [& messages]
    (throw (UnsupportedOperationException. (apply str messages))))

  (defn illegal-argument! 
    "throw a java.lang.IllegalArgumentException with messages as text."
    [& messages]
    (throw (IllegalArgumentException. (apply str messages)))))

;; sample draw function
(defn empty-draw
  "An example of a function which sets the screen white"
  []
  ;(fill 226)
  ;(background-float 0 0 0)
  ;(fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  ;(ellipse 100 100 (rand-int 90) (rand-int 90))
  ;(stroke-float 10)
  ;(line 10 10 (+ 200 (rand-int 1000)) (+ 200 (rand-int 500)))
  ;(no-stroke)
  ;(filter-kind INVERT)
  )

(def draw empty-draw)

;; sample applet implementation
(defn make-applet []
  (proxy [PApplet] []
    (setup [] ;; default setup method
           (binding [*applet* this]
             ;(let [parent-size (.getSize (.getParent this))]
             ;  (size (.width parent-size) (.height parent-size) P3D))

             ;; initial size, P3D is the (fast) renderer
             (size 100 100 P3D)
             (smooth)
             (no-stroke)
             (fill 0)
             (framerate 10)))
    (draw [] ;; default draw method
          (binding [*applet* this]
            (draw)))))


(defn setup-processing
  "Create a swing Frame, run a processing.core.PApplet applet
  inside and return the created frame."
  {:arglists '([applet & :size [width height] :smooth true])}
  [applet & opts]
  (let [{[width height] :size} (as-keyargs opts {:size [200 200]})
        swing-frame (JFrame. "cljtest")]
    (.init applet)
    (doto swing-frame
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize width height)
      (.add applet)
      (.pack)
      (.setVisible true))
    swing-frame))

;; convenient constructors

(defn vec2
  "Make a org.jbox2d.common.Vec2 from a clojure vector or two numbers."
  ([v] (Vec2. (v 0) (v 1)))
  ([x y] (Vec2. x y)))

(defn make-aabb
  "Make a org.jbox2d.collision.AABB (axis-aligned-bounding-box) 
x1,y1
  +-------+       from   [x1 y1 x2 y2] or
  | AABB  |              [x1 y1] [x2 y2] or 
  +-------+              x1 y1 x2 y2
        x2,y2"
  ([v]
     (AABB. (vec2 (v 0) (v 1)) (vec2 (v 2) (v 3))))
  ([lower upper]
     (AABB. (vec2 lower) (vec2 upper)))
  ([a b c d]
     (AABB. (vec2 a b) (vec2 c d))))

;; bodies & shapes

(defn get-shape-type [clojure-shape-def]
  (cond (= (count clojure-shape-def) 1) :circle
        (= (count clojure-shape-def) 2) :box
        :else :poly))

(defn make-shape-def 
  "Create a ShapeDef from args which is a hashmap. Argmap:
  :shape x     -> radius for :circle
         [x y] -> :box
         [[x y] [x y] [x y]+] -> :poly
  :angle 0
  :center [0 0]
  :friction 0.3
  :density 1.0"
  [args]
  (let [{:keys [shape]} args
        shape-def (condp = (get-shape-type shape)
                    :box (doto (PolygonDef.)
                           (.setAsBox (shape 0)
                                      (shape 1) 
                                      (vec2 (:center args [0 0]))
                                      (:angle args 0.0)))
                    (throw (unsupported-operation! "not implemented")))]
    (set! (.density shape-def) (:density args 1.0))
    (set! (.friction shape-def) (:friction args 0.3))
    shape-def))

(defn make-body-def
  "creates a body def from a shape def and some additional args.
  hash-map args:
    :pos [x y]
    :angle 0
    :is-bullet false, allow for more perf, set true for fast-moving bodies
    :allow-sleep true"
  [shape-def args]
  (let [{:keys [pos angle bullet allow-sleep]} args]
    (let [b (BodyDef.)]
      (-> b .position (.set (-> args :pos vec2)))
      (set! (.isBullet b) (:bullet args false))
      (set! (.allowSleep b) (:allow-sleep args true))
      b)))

;;(defn make-box-draw-fn
;;  "Returns a function which creates a function which draws
;;  the body using processing."
;;  [body]
;;  (let [box-verts (.getVertices (.getShapeList body))];; returns a single shape, use .getNext to iterate
;;    (fn [] ;; call this function in the game thread to get a draw function
;;      (let [[v1 v2 v3 v4] (map #(.getWorldPoint body %) box-verts)]
;;        (fn [] ;; use this fn to draw the body
;;          (quad (.x v1) (.y v1)
;;                (.x v2) (.y v2)
;;                (.x v3) (.y v3)
;;                (.x v4) (.y v4)))))))



(defn make-body
  "Create a body in the world using opts and put
  name (keyword/symbol) in its user slot.
  See make-shape-def and make-body-def for valid arg-keys.
  additionally:
    :dynamic true/false  when false, make an unsimulated (ground) body."
  ([world name args]
     (let [sd (make-shape-def args)
           bd (make-body-def sd args)
           body (.createBody world bd)]
       (.createShape body sd)
       (if (:dynamic args true) (.setMassFromShapes body))
       (.setUserData body name)
       body)))

;(def body (make-box (:world @sim) {:pos [5 5] :shape [0.5 0.5]}))

(defnk create-world
  [:lower [-200.0 -100.0]
   :upper [ 200.0  200.0]
   :gravity [ 0.0  10.0]]
  (let [aabb (AABB. (vec2 lower) (vec2 upper));; axis-aligned-bounding-box
        gravity (vec2 gravity)
        sleep true]
    (World. aabb gravity sleep)))

(defmethod print-method Vec2 [vec2 w]
  (.write w "[")
  (print-method (.x vec2) w)
  (.write w " ")
  (print-method (.y vec2) w)
  (.write w "]"))

(defmethod print-dup Vec2 [vec2 w]
  (.write w "#=(org.jbox2d.common.Vec2. ")
  (print-method (.x vec2) w)
  (.write w " ")
  (print-method (.y vec2) w)
  (.write w ")"))



;; (:world sim) is inherently thread-unsafe, it runs in its own
;; background-game loop 60 times a second; 
;; to inspect the state of simulated objects, we need a function to
;; copy and return all the bodies within a AABB
;; (axis-aligned-bounding-box)
(defn start-world-thread
  "... and return a function to add tasks to the world."
  [init-world-fn frequency]
  (let [iterations 10
        physics-tasks (ref [])]
    (background-periodically (fn [world]
                               ;;(.step world (/ 1 frequency) iterations) ;; run the physics sim for 1/freq second
                               (doseq [t (dosync (do1 @physics-tasks (ref-set physics-tasks [])))]
                                 (t world))) ;; run actions on this world, e.g. to observe body state, observer body state by copying it into clojure datastructures
                             frequency 
                             :init-fn init-world-fn
                             :name 'jbox-physics)
    (fn [& fs]
      (dosync (commute physics-tasks (partial reduce conj) fs)))))

(def sim (ref {:world-accessor nil
               :frame nil
               :applet nil
               :camera [[0 0] 1] ;; [offset from center, scale]??
               }))

(defn ref-assoc [map & kvs]
  (ref-set map (apply assoc @map kvs)))

(defmacro with-world
  "execute body in the world thread, wait for its result and return it.
Waits no more than 2 seconds for the result and returns nil on timeout."
  [& body]
  `(let [ret# (java.util.concurrent.ArrayBlockingQueue. 1)]
     ((:world-accessor @sim)
      (fn [~'world]
        (let [result# (do ~@body)]
          (.add ret# [result#])))) ;; wrap result in vector, to be able to return nil
     (if-let [return-val# (.poll ret# 2 (timeunit :sec))]
       (first return-val#))))

(defmacro with-applet [& body]
  `(binding [*applet* (:applet @sim)]
     ~@body))

(defn init []
  (let [physics-frames 60.0
        render-frames 25.0
        app (make-applet)
        frm (setup-processing app :size [320 200]) ;; creates its own render thread, returns the applet
        wac (start-world-thread 
             #(create-world)
             physics-frames)]
    (.addWindowListener frm (proxy [WindowAdapter] []
                              (windowClosed [e] ;; stop the physhics sim when closing the frame
                                            (wac (fn [_] (interrupt))))))
    (dosync (ref-assoc sim :world-accessor wac ;; for safely modifying and reading
                           :frame frm
                           :applet app))))


(defn get-bodies
  "Return an array of jbox2d Shapes."
  ([world]
     (get-bodies world (.getWorldAABB world)))
  ([world aabb]
     (.query world aabb (.getBodyCount world))))

(defn get-shape-points
  "given a body, return a seq of its shape corners 
  in world-coordinates."
  [shape] ;; for simple shapes
  (let [verts (.getVertices shape) ;;(.getShapeList shape)
        body (.getBody shape)]
    (map #(.getWorldPoint body %) verts)))

;; jbox objects -> clojure generic datastructures
(defn query-world
  ([world]
     (map 
      get-shape-points
      (get-bodies world))))

;(with-world
;  (query-world world))

;;bodies (query-world world) ;; blocks

(defn my-draw [points-vec]
  (let [[offset, zoom] (:camera @sim)]
    (translate offset)
    (scale zoom)
    (doseq [[v1 v2 v3 v4] points-vec] ;; assume boxes      
      (quad (.x v1) (.y v1)
            (.x v2) (.y v2)
            (.x v3) (.y v3)
            (.x v4) (.y v4)))))


(defn set-zoom [zoom]
  (dosync (alter sim assoc-in [:camera 1] zoom)))

(defn set-pos [x y]
  (dosync (alter sim assoc-in [:camera 0] [x y])))

(defn clear-world
  "remove all objects from world"
  [world]
  
  )

(comment
;; test run
  (do (init)
      (with-world 
       (make-body world 'ground
                  {:shape [10 1]
                   :pos [0 0]
                   :dynamic false})
       (make-body world 'box1
                  {:shape [1 1]
                   :pos [0 -10]
                   :dynamic true}))

      (set-zoom 6)
      (set-pos 100 100)

      (with-applet
        (stroke-float 0)
        (fill 255)
        (background-int 255)
        (my-draw (with-world (query-world world)))))

  (dotimes [n 100]
    (with-world (.step world (/ 1 60.0) 10))
    (with-applet 
     (stroke-float 0)
     (fill 255)
     (background-int 255)
     (my-draw (with-world (query-world world))))
    (Thread/sleep 100))

)



(comment

(init)

;; test processing
(with-applet
  (fill 226)
  (background-float 0 0 0)
  (fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  (ellipse 100 100 (rand-int 90) (rand-int 90))
  (stroke-float 10)
  (line 10 10 (+ 200 (rand-int 1000)) (+ 200 (rand-int 500)))
  (no-stroke)
  (filter-kind INVERT)
  (ellipse 50 50 (rand-int 90) (rand-int 90)))

((@sim :world-accessor) #(println "hello" %) (fn [_] (println "world")))

(with-world
  (println world)
  'aaaa)


(with-world
  (try   
   ;;(get-bodies world)
   (make-body world 'name
              {:shape [1 1]
               :pos [0 0]})
   (catch Exception e e)))

(def xxx
     (with-world
       (seq (get-bodies world))))

(-> (get-shape-points (first  xxx)) first type)
(.getNext (first  xxx))
org.jbox2d.collision.PolygonShape



(pprint (ps))

(.size (@sim :applet))
((@sim :world-accessor) #(println (seq (get-bodies %))))

;; quit physics
(map interrupt '(jbox-physics Animation))


;; draw-fn
(defn draw-fn []
  (let [[[cam-x cam-y] scale-factor] (@sim :camera)]
    (scale scale-factor)
    (translate cam-x cam-y)

    ))

((@sim :world-accessor)
 #(def tempworld %))

)





;; jbox objects
;; processing objects


;object
; |-- appearance on screen: bitmap, vectorimage
; |-- physical definition: ShapeDef, BodyDef
;       |-- position, angle

;world
; |-- viewport: the thing processing draws
;      o the window + scale factor + offset

; |-- whole-world: the thing that actually gets simulated
;      o usually 400x400

; |-- physics-thread:
;      o 60 frames per second steady phyiscs simulation
;      o own game-loopy thread

; |-- render-thread
;      o 25 frames per second drawing a snapshot of world,
;      o keeping track of offset and zoom (= viewport)              
;      o processing: draw method
