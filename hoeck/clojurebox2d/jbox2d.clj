

;; jbox related stuff

(ns hoeck.clojurebox2d.jbox2d
  (:use (clojure.contrib pprint def)
        hoeck.iterate)
  (:import (org.jbox2d.common Color3f Settings Vec2)
           (org.jbox2d.collision ContactID AABB
                                 MassData)
           (org.jbox2d.collision.shapes Shape PolygonDef CircleDef  ShapeDef
                                        PolygonShape CircleShape)
           (org.jbox2d.dynamics Body BodyDef BoundaryListener ContactListener
                                DebugDraw DestructionListener World)
           (org.jbox2d.dynamics.contacts ContactResult)
           (org.jbox2d.dynamics.joints Joint MouseJoint MouseJointDef)
           
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue TimeUnit)
           (java.util HashMap)))

(defn unsupported-operation!
  "throw a java.lang.UnsupportedOperationException with messages as text."
  [& messages]
  (throw (UnsupportedOperationException. (apply str messages))))

(defn illegal-argument!
  "throw a java.lang.IllegalArgumentException with messages as text."
  [& messages]
  (throw (IllegalArgumentException. (apply str messages))))

(defn vec2
  "Make a org.jbox2d.common.Vec2 from a clojure vector or two numbers.
  If v is already a Vec2, return it."
  ([v] (if (isa? (class v) Vec2)
         v
         (Vec2. (v 0) (v 1))))
  ([x y] (Vec2. x y)))

(defn vec2->clj
  "Make a clojure vector from a jbox2d Vec2."
  [#^Vec2 v] [(.x v) (.y v)])

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

(defn- get-shape-type [clojure-shape-def]
  (cond (not (vector? clojure-shape-def)) :circle
        (= (count clojure-shape-def) 2) :box
        :else :poly))

(defn make-shape-def 
  "Create a ShapeDef from args which is a hashmap. Argmap:
  :shape x     -> radius for :circle
         [x y] -> width-height for :box
         [[x y] [x y] [x y]+] -> many vecs for :poly
  :angle 0
  :center [0 0] (only for :box)
  :friction 0.3
  :density 1.0
  :restitution 0.3

  Shapes must be convex."
  [args]
  (let [{:keys [shape]} args
        shape-def (condp = (get-shape-type shape)
                    :circle (let [cd (CircleDef.)]
                              (set! (.radius cd) shape)
                              cd)
                    :box (doto (PolygonDef.)
                           (.setAsBox (shape 0)
                                      (shape 1) 
                                      (vec2 (:center args [0 0]))
                                      (:angle args 0.0)))
                    :poly (let [pd (PolygonDef.)]
                            (doseq [v shape]
                              (.addVertex pd (vec2 v)))
                            pd)
                    (throw (unsupported-operation! "not implemented")))]
    (set! (.density #^ShapeDef shape-def)  (:density args 1.0))
    (set! (.friction #^ShapeDef shape-def) (:friction args 0.3))
    (set! (.restitution #^ShapeDef shape-def) (:restitution args 0.3))
    shape-def))

(defn make-body-def
  "creates a body def from a shape def and some additional args.
  hash-map args:
    :pos [x y]
    :angle 0
    :is-bullet false, allow for more perf, set true for fast-moving bodies
    :allow-sleep true
    :linear-damping 0.0
    :angular-damping 0.0"
  [shape-def args]
  (let [{:keys [pos angle bullet allow-sleep]} args]
    (let [b (BodyDef.)]
      (-> b .position (.set (-> args (:pos [0 0]) vec2)))
      (set! (.angle b)          (:angle args 0))
      (set! (.isBullet b)       (:bullet args false))
      (set! (.allowSleep b)     (:allow-sleep args true))
      (set! (.linearDamping b)  (:linear-damping args 0.0))
      (set! (.angularDamping b) (:angular-damping args 0.0))
      b)))

(defn make-body
  "Create a body in the world using opts and set the given userdata on it.
  See make-shape-def and make-body-def for valid arg-keys.
  additionally:
    :dynamic true   when false, make an unsimulated (ground) body."
  ([#^World world userdata args]
     (let [sd (make-shape-def args)
           bd (make-body-def sd args)
           body (.createBody world bd)]
       (.createShape body sd)
       (if (:dynamic args true) (.setMassFromShapes body))
       (.setUserData body userdata)
       body)))

;; useful for debris
(defn make-body-ctor
  "Returns a function creating a Body:
  (fn body-ctor [world userdata]) which returns the created Body.
  Memoizes shape and body definitions to allow a faster Body instantiation.
  Use only in the jbox2d thread."
  [args]
  (let [sd (make-shape-def args)
        bd (make-body-def sd args)
        dynamic? (:dynamic args true)]
    (fn body-ctor [#^World world userdata]
      (let [body (.createBody world bd)]
        (.createShape body sd)
        (if dynamic? (.setMassFromShapes body))        
        (.setUserData body userdata)
        body))))

(defstruct #^{:private true} body-struct
  :pos :angle :linear-velocity :angular-velocity
  :userdata)

(defn body->clj 
  "Convert a jbox2d Body into a clojure hashmap, eg. to share it across
  Threads."
  [#^Body b]
  (create-struct 
   (-> b .getPosition vec2->clj)
   (.getAngle b)
   (-> b .getLinearVelocity vec2->clj)
   (.getAngularVelocity b)
   (.getUserData b)))

(defn make-mass
  "Creates a jbox2d MassData object from the given mass and inertia.
  The position, a vector describing an offset from the bodys center
  defaults to [0 0]."
  ([mass inertia] (make-mass mass inertia [0 0]))
  ([mass inertia pos]
     (let [md (MassData.)]
       (set! (. md center) (vec2 pos))
       (set! (. md I) inertia)
       (set! (. md mass) mass)
       md)))

;(def body (make-box (:world cbox) {:pos [5 5] :shape [0.5 0.5]}))

(defn make-world
  "Create a jbox2d world using lower and upper to create the worlds
  AABB (see make-aabb)"
  [opts]
  (let [{:keys [upper lower gravity]
         :or {lower [-200.0 -100.0]
              upper [ 200.0  200.0]
              gravity [ 0.0   10.0]}} opts
        aabb (AABB. (vec2 lower) (vec2 upper)) ;; axis-aligned-bounding-box
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


(def #^World *world*)
(def #^HashMap *state*)

(defn jbox2d-loop [tick]) ;; default function, does nothing

;; jbox2d World is thread-unsafe
;; -> thread-confinement:
;; world runs in its own thread and is accessed only from within this thread
(defn make-jbox2d-thread
  "... and return a atom to swap in a worker function.
  Jbox2d.World is thread-unsafe. World and World methods should only
  be accessed from within this thread."
  [opts]
  (let [frequency (:thread-frequency opts 60)
        periodic-time (long (/ 1000000000 frequency));; in nanoseconds
        ;;job (atom (constantly nil)) ;; function executed at every step
        ]
    (Thread. #^Runnable (fn [] (binding [*world* (make-world opts)
                                         ;; mutable state, keep references to body etc.
                                         *state* (java.util.HashMap.)]
                                 (try (loop [start (System/nanoTime)
                                             tick 0]
                                        (jbox2d-loop tick)
                                        ;; sleep the remaining time or continue immediately
                                        (.sleep TimeUnit/NANOSECONDS (- periodic-time (- (System/nanoTime) start)))
                                        (recur (System/nanoTime) (inc tick)))
                                      
                                      ;; quit thread on intertuption
                                      (catch InterruptedException e
                                        (println "jbox-physics interrupted.")))))
             "jbox-physics")))

;; more utils

(defn query
  "Return an array of shapes which potentially overlap with the given aabb.
  aabb defaults to the world aabb."
  ([#^World world] 
     (query world (.getWorldAABB world)))
  ([world v0 v1]
     (query world (make-aabb v0 v1)))
  ([world x0 y0 x1 y1]
     (query world (make-aabb x0 y0 x1 y1)))
  ([#^World world aabb]
     (.query world aabb (.getBodyCount world))))

(defn get-shape-points
  "given a polygon shape, return a seq of its shape corners
  in world-coordinates."
  [#^PolygonShape shape] ;; for polygon shapes only!
  (iter (let #^Body body (.getBody shape))
        (for vert in-array (.getVertices shape))
        (collect (vec2->clj (.getWorldPoint body vert)))))

(defn get-circle-points
  "Given a circle shape, return its center in world coordinates and
  its radius: [center, radius]"
  [#^CircleShape shape]
  (vector (vec2->clj (.getWorldPoint (.getBody shape) (.getLocalPosition shape)))
          (.getRadius shape)))

(defn clear-world
  "remove all objects from world"
  [#^World world]
  (iter (for b call .getNext on (.getBodyList world))
        (do (.destroyBody world b)
            (recur))))

(defn body-info 
  "Return a vector of userdata of all bodies."
  [#^World world]
  (iter (for shape in-array (query world))
        (for body as (.getBody shape))
        (collect (.getUserData body))))