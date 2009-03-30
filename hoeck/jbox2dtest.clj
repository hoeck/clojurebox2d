

(ns hoeck_misc.jbox2d
  (:use rosado.processing
        com.infolace.format
        hoeck.library
        hoeck_misc.thread)
  (:import (org.jbox2d.common Color3f Settings Vec2)
           (org.jbox2d.collision PolygonDef CircleDef ContactID Shape AABB)
           (org.jbox2d.dynamics Body BodyDef BoundaryListener ContactListener
                                DebugDraw DestructionListener World)
           (org.jbox2d.dynamics.contacts ContactResult)
           (org.jbox2d.dynamics.joints Joint MouseJoint MouseJointDef)
           
           (processing.core PApplet)

           (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter)           
           (javax.swing JFrame JLabel JTextField JButton)))


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
             (fill 55555)
             (framerate 10)))
    (draw [] ;; default draw method
          (binding [*applet* this]
            (empty-draw)))))


(defn setup-processing
  "Create a swing Frame, run a processing.core.PApplet applet
  inside and return the created frame."
  {:arglists '([applet & :size [width height] :smooth true])}
  [applet & opts]
  (let [{[width height] :size} (as-keyargs opts {:size [200 200]})
        swing-frame (JFrame. "Processing with Clojure")]
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

(defn make-shape-def 
  "Create a ShapeDef from args which is a hashmap."
  {:arglists '([:type (:box or :circle or :poly)
                :shape ([x y]* or a radius for :circle)
                :angle 0
                :center [0 0]              
                :friction 0.3
                :density 1.0])} ;; keyargs-definition
  [args]
  (let [{:keys [shape]} args
        shape-def (condp = (:type args :box)
                    :box (doto (PolygonDef.)
                           (.setAsBox (shape 0)
                                      (shape 1) 
                                      (vec2 (:center args [0 0]))
                                      (:angle args 0.0))))]
    (set! (.density shape-def) (:density args 1.0))
    (set! (.friction shape-def) (:friction args 0.3))
    shape-def))


(defn make-body-parts
  "Creates a BodyDef and a ShapeDef from args which is a hashmap."
  {:arglists '([:pos [x y]
                :shape [x y] or [x y [center] angle] or radius or [center [x y] [x y] [x y]+]                
                :is-bullet true
                :allow-sleep true])}
  ([args]
     (let [body-def (let [b (BodyDef.)]
                      (-> b .position (.set (vec2 (:pos args))))
                      (set! (.isBullet b) (:is-bullet args true))
                      (set! (.allowSleep b) (:allow-sleep args true))
                      b)
           shape-def (make-shape-def args)]
       [body-def shape-def])))


(defn make-box-draw-fn
  "Returns a function which creates a function which draws
  the body using processing."
  [body]
  (let [box-verts (.getVertices (.getShapeList body))];; returns a single shape, use .getNext to iterate
    (fn [] ;; call this function in the game thread to get a draw function
      (let [[v1 v2 v3 v4] (map #(.getWorldPoint body %) box-verts)]
        (fn [] ;; use this fn to draw the body
          (quad (.x v1) (.y v1)
                (.x v2) (.y v2)
                (.x v3) (.y v3)
                (.x v4) (.y v4)))))))

(defn make-box
  "Create a box in the world using opts.
  :dynamic true/false  when false, make an unsimulated (ground) body.
  Hands opts over to `make-body-parts'."
  {:arglists (list (vec (concat (first (:arglists (meta #'make-body-parts)))
                                [:dynamic true])))}
  ([world opts]
     (let [[bd sd] (make-body-parts opts)
           box (.createBody world bd)]
       (.createShape box sd)
       (if (:dynamic opts true) (.setMassFromShapes box))
       (.setUserData box {:name (:name opts 'box) :draw-fn (make-box-draw-fn box)})
       box)))


;(def body (make-box (:world @sim) {:pos [5 5] :shape [0.5 0.5]}))

(defn create-world
  {:arglists '([:lower [x,y] :upper [x,y] :gravity [0 -10]])}
  ([] (create-world {}))
  ([args]
     (let [args (merge {:lower [-200.0 -100.0]
                        :upper [200.0 200.0]
                        :gravity [0.0 -10.0]}
                       args)           
           aabb (AABB. (vec2 (:lower args)) (vec2 (:upper args))) ;; axis-aligned-bounding-box
           gravity (vec2 (:gravity args))
           sleep true]
       (World. aabb gravity sleep))))

;(def simple-test
;     (let [timestep (/ 1 60.0)
;           iterations 10
;           w (create-world)
;           g (create-box w {:shape [50 50] :pos [0,0] :density 0.0 :dynamic false})
;           b (create-box w {:shape [1 1] :pos [0 5] :density 1 :friction 0.3})]
;       {:world w
;        :step #(.step w timestep iterations)
;        :body b
;        :bkoords #(vector (.getPosition b)
;                          (.getAngle b))}))

;(defn get-bodies
;  "Return a lazy seq of bodies from a World."
;  [world]
;  (let [first-body (.getBodyList world)]
;    (take-while identity
;                (iterate #(.getNext %) first-body))))

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
  [world frequency]
  (let [iterations 10
        physics-tasks (ref [])]
    (background-periodically (fn []
                               ;;(.step world (/ 1 frequency) iterations) ;; run the physics sim for 1/freq second
                               (doseq [t (dosync (do1 @physics-tasks (ref-set physics-tasks [])))]
                                 (t world)));; run actions on this world, e.g. to observe body state
                             frequency
                             'jbox-physics)
    (fn [& fs]
      (dosync (commute physics-tasks (partial reduce conj) fs)))))

(def sim (ref {:world-accessor nil
               :frame nil
               :applet nil
               :camera [[0 0] 1] ;; [offset from center, scale]??
               }))

(defn ref-assoc [map & kvs]
  (ref-set map (apply assoc @map kvs)))

(defn init []
  (let [physics-frames 60
        render-frames 25
        wld (create-world)
        app (make-applet)
        frm (setup-processing app :size [320 200]) ;; creates its own render thread, returns the applet
        wac (start-world-thread wld physics-frames)]
    (.addWindowListener frm (proxy [WindowAdapter] [] 
                              (windowClosed [e] ;; stop the physhics wim when closing the frame
                                            (wac (fn [_] (interrupt))))))
    (dosync (ref-assoc sim
                       :world wld ;; read static stuff from here (like size)
                       :world-accessor wac ;; for safely modifying and reading
                       :frame frm
                       :applet app))))


;(init)
;((@sim :world-accessor) #(println "hello" %) (fn [_] (println "world")))

;(map interrupt '(jbox-physics Animation))

;(pprint (ps))




;(.size (@sim :applet))


;; draw-fn
(defn draw-fn []
  (let [[[cam-x cam-y] scale-factor] (@sim :camera)]
    (scale scale-factor)
    (translate cam-x cam-y))
  
  )

((@sim :world-accessor)
 #(def tempworld %))

(def
 
 )


(defn get-bodies
  ([world]
     (get-bodies world (.getWorldAABB world)))
  ([world aabb]
     (.query world aabb (.getBodyCount world))))

(defn extract-bodies
  ([shapes]
     (map #(let [b (.m_body %)] 
             [(.position ) m_userdata]
              shapes)))






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
