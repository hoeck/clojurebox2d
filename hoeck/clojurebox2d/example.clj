
(ns hoeck.clojurebox2d.example
  (:use hoeck.clojurebox2d
        hoeck.iterate
        rosado.processing
        clojure.contrib.pprint
        clojure.contrib.def
        hoeck.thread)
  (:import (java.util HashMap LinkedList ArrayDeque)

           (org.jbox2d.collision.shapes Shape)
           (org.jbox2d.dynamics Body World ContactListener)
           (org.jbox2d.dynamics.contacts ContactPoint)))

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
  [#^Shape shape]
  (let [body (.getBody shape)]
    (vector (if (isa? (class shape) org.jbox2d.collision.shapes.PolygonShape) 
              (get-shape-points shape)
              ;; else: circleShape
              (get-circle-points shape))
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

(defn default-style []
  (stroke-float 0)
  (fill 255))

(defn box-draw [[[x0 y0] [x1 y1] [x2 y2] [x3 y3]]]
  (fill 0)
  (quad x0 y0
        x1 y1
        x2 y2
        x3 y3))

(defn circle-draw [[[x y] r]]
  (fill 255)
  (let [d (* 2 r)]
    (ellipse x y d d)))

(defn planet-draw [[[x y] r]]  
  ;; draw the planet a little bit larger than it actually is
  (fill 255)
  (let [d (* 2.2 r)]
    (ellipse x y d d)))

(defn selected-box-draw [shape-points]
  (fill 0) ;; black
  (box-draw shape-points))

(defn draw-hp-bar [left-or-right hp]
  (let [width (. *applet* width)
        height (. *applet* height)
        [x0 y0] (if (= left-or-right :left) 
                  [5 (- height 15)]
                  [(- width 45) (- height 15)])
        [w h] [40 10]]
    (fill 255)
    (stroke-float 0)
    (rect-mode CORNER)
    (rect x0 y0 w h)
    (if (= :left left-or-right) 
      (fill-float 255 0 0 125)
      (fill-float 0 0 255 125))
    (rect x0 y0 (* hp w) h)))

(declare current-player-hitpoints max-player-hitpoints)
(defn draw-osd []
  (text-mode SCREEN)
  (fill 0)
  ;;(string->text "foobar" 10 10)
  (let [[hp1 hp2] @current-player-hitpoints]
    (draw-hp-bar :left (- 1 (/ hp1 (inc max-player-hitpoints))));; player-1
    (draw-hp-bar :right (- 1 (/ hp2 (inc max-player-hitpoints))))))

(defn draw-background []
  (background-int 255)  
  (draw-osd))

(defn my-draw
  "Draws all body shapes (only boxes for now) using quad."
  [world-state]
  ;; background-art
  (draw-background)
  ;; camera transformations
  (let [[offset, zoom] @my-camera]
    (translate offset)
    (scale zoom))  
  ;; drawing bodies
  (doseq [[shape-data name draw-fn] world-state]
    (if draw-fn
      (draw-fn shape-data)))
   ;; need no zooming for fonts
  )


;; center the current world on resize
;; adjust zoom so that the whole world fits in the frame
(declare wrap-world-box)

(redef window-resized
  (let [[x0 y0 x1 y1] wrap-world-box
        world-width (+ (Math/abs x0) x1)
        world-height (+ (Math/abs y0) y1)
        dimension (.getSize #^javax.swing.JFrame (:frame cbox))
        width (. dimension width)
        height (. dimension height)
        zoom (min (/ width world-width)
                  (/ height world-height))]
    (set-my-camera-pos (int (/ width 2))
                       (int (/ height 2)))
    (set-my-camera-zoom zoom)))

;; bodies

(defn bodies-info []
  (with-world 
    (map #(body-userdata #^Body %)
         (set (map #(.getBody #^Shape %)
                   (query world))))))

(defnk make-simple-box [world body-name
                        :shape [1 1]
                        :pos [0 0]
                        :dynamic true
                        :angular-damping 0.0
                        :linear-damping 0.0
                        :draw-fn box-draw]
  (make-body world (make-body-userdata :name body-name 
                                       :draw-fn draw-fn)
             {:pos pos
              :dynamic dynamic
              :shape shape
              :angular-damping angular-damping
              :linear-damping linear-damping}))

(defnk make-simple-circle [world body-name
                           :radius 1
                           :pos [0 0]
                           :dynamic true
                           :angular-damping 0.0
                           :linear-damping 0.0
                           :friction 0.3
                           :draw-fn circle-draw]
  (make-body world (make-body-userdata :name body-name
                                       :draw-fn draw-fn)
             {:pos pos
              :dynamic dynamic
              :shape radius
              :angular-damping angular-damping
              :linear-damping linear-damping
              :friction friction}))

(defn player-1-draw [[[x0 y0] [x1 y1] [x2 y2]]]
  (fill-float 255 0 0 125)
  (triangle x0 y0 x1 y1 x2 y2))

(defn player-2-draw [[[x0 y0] [x1 y1] [x2 y2]]]
  (fill-float 0 0 255 125)
  (triangle x0 y0 x1 y1 x2 y2))

(defn make-player-body [world name draw-fn body-def-args]
  (make-body world (make-body-userdata :name name
                                       :draw-fn draw-fn)
             (merge {:dynamic true
                     :shape [[-1.1 -1.5] [1.1 -1.5] [0 1.1]]
                     :angular-damping 0
                     :linear-damping 0}
                    body-def-args)))

(comment
  (with-world (make-player-body world 'testee player-1-draw {:pos [10 10]}))
  (bodies-info)
  (with-world (seq (.getVertices (.getShapeList (.get state 'player-1))))))

;; central gravity generator

(def planet-mass 5000)

(defn apply-gravity [#^World world tick state]
  (iter (for #^Body body call .getNext on (.getBodyList world))
        (do (when (not (.isStatic body))
              ;; assume the center of gravity is [0,0]
              (let [r-vec (.getWorldCenter body) ;; vector from [0,0] to body
                    ;; simple gravity function: f = (/ (* m1 m2) (sqr r))
                    r2 (max 0.0001 (.lengthSquared r-vec))
                    gravity-force (/ (* planet-mass (.getMass body)) r2)
                    r-norm (doto (.negate r-vec) (.normalize))
                    force-vec (.mul r-norm gravity-force)]
                (.applyForce body
                             force-vec
                             r-vec)))
            (recur))))

(comment (with-world (apply-gravity world tick state)))

;; player-control job

(defn turn [#^Body player left-or-right amount]
  (.applyTorque player
                (if (= left-or-right :left)
                  (* -1 amount)
                  amount)))

(defn thrust [world #^Body player amount]
  (.applyForce player
               (.mul (.getWorldVector player (vec2 [0 1])) amount)
               (.getWorldCenter player)))

(declare shoot)
(declare restart-game)

(defn player-control [world tick #^HashMap state]
  (when (= 0 (rem tick 2))
    (let [player-1 (.get state 'player-1)
          player-2 (.get state 'player-2)
          thrust-amount 110
          break-amount (* -0.2 thrust-amount)
          turn-amount 20
          ks @hoeck.clojurebox2d.processing/keystatus]
      ;; player 1
      (when (not (.get (.get state player-1) 'dead))
        (when (:up ks) (thrust world player-1 thrust-amount))
        (when (:down ks) (thrust world player-1 break-amount))
        (when (:left ks) (turn player-1 :left turn-amount))
        (when (:right ks) (turn player-1 :right turn-amount))
        (when (:space ks) (shoot world tick state player-1)))
      ;; player 2
      (when (not (.get (.get state player-2) 'dead))
        (when (:w ks) (thrust world player-2 thrust-amount))
        (when (:s ks) (thrust world player-2 break-amount))
        (when (:a ks) (turn player-2 :left turn-amount))
        (when (:d ks) (turn player-2 :right turn-amount))
        (when (:q ks) (shoot world tick state player-2))))))

;; bullets

(defn shoot [world tick #^HashMap state #^Body player] ;; the player-body
  (let [#^HashMap player-data (.get state player)
        ;; use a queue to add bullets at the end, so that the bullet-gc job
        ;; removes them from the head when their lifetime is over
        #^ArrayDeque player-bullets (or (.get player-data 'bullets)
                                        (let [q (ArrayDeque.)]
                                          (.put player-data 'bullets q)
                                          q))
        player-bullets-count (.size player-bullets)
        player-bullets-last-shot (nth (or (.peekLast player-bullets) [nil 0]) 1)]
    (when (and (< player-bullets-count 6) ;; max X bullets
               (< 3 (- tick player-bullets-last-shot))) ;; shoot delay in ticks
      (let [p-pos (.getPosition player)
            bullet-pos (.getWorldPoint player (vec2 0 2.5)) ;; in front of the spacecraft
            #^Body bullet (make-simple-circle world
                                              (str 'bullet- (body-userdata player :name) "-" tick) ;; generate a unique bullet name to identify objects across the network line
                                              :pos (vec2->clj bullet-pos)
                                              :radius 0.6
                                              :linear-damping 1.5)]
        ;; initial bullet speed
        (.setLinearVelocity bullet (.getLinearVelocity player))
        (.applyImpulse bullet (.getWorldVector player (vec2 [0 120])) bullet-pos)

        ;; bullet bookkeeping
        (.add player-bullets [bullet tick])))))

(comment (with-world (shoot world tick state (.get state 'player-1))))

(def bullet-lifetime-ticks 70) ;; in ticks

(defn player-bullet-gc [#^World world tick #^HashMap state player]
  (let [#^HashMap player-data (.get state player)
        #^ArrayDeque player-bullets (or (.get player-data 'bullets) (ArrayDeque.))]
    (while (and (.peek player-bullets)
                (let [[b t] (.peek player-bullets)]                  
                  (if (< bullet-lifetime-ticks (- tick t))
                    (do (.destroyBody world b)
                        (.remove player-bullets)
                        true)
                    false))))))

(defn bullet-gc [world tick #^HashMap state] ;; remove old bullets
  (when (= 0 (rem tick 5)) ;; every 5 ticks
    (player-bullet-gc world tick state (.get state 'player-1))
    (player-bullet-gc world tick state (.get state 'player-2))))

(comment (with-world (bullet-gc world tick state)))
(comment (pprint (with-world (.get state (.get state 'player-1)))))
(comment (pprint (with-world (.get state 'player-bullets))))
(comment (pprint (with-world (.put state 'player-bullets 0))))
(comment (pprint (bodies-info)))

(def wrap-world-box [-70 -50 70 50])

(defn wrap-world-job [#^World world tick state]
  (iter (let [x0 y0 x1 y1] wrap-world-box)
        (for #^Body b call .getNext on (.getBodyList world))
        (do (let [pos (.getPosition b)
                  px (.x pos)
                  py (.y pos)
                  newx (cond (< px x0) x1
                             (< x1 px) x0)
                  newy (cond (< py y0) y1
                             (< y1 py) y0)]
              (when (or newx newy)
                (.setXForm b
                           (vec2 (or newx px) (or newy py))
                           (.getAngle b)))
              (recur)))))

;; contacts & damage

(defn bounce-player [#^Body player]
  (let [direction (doto (.getWorldCenter player) (.normalize))]
    (.applyImpulse player (.mul direction 40) (.getWorldCenter player))))

(comment (with-world (bounce-player (.get state 'player-1))))

(defn player-contact [state player other-body-name other-body]
  (let [player-data (.get state player)]
    (when (and (string? other-body-name) (.startsWith #^String other-body-name "bullet"))
      (println :collide (body-userdata player :name))
      ;; player player-data hit by bullet -> compute damage
      (let [pdamage (or (.get player-data 'damage) 0)]
        (.put player-data 'damage (inc pdamage)))
      ;; mark the bullet for destruction ???
      
      )
    (when (= other-body-name 'planet)
      ;; give extra boost
      ;;(bounce-player player)
      )))

(defn contact-add [#^ContactPoint cp #^HashMap state]
  (let [a (.getBody (.shape1 cp))
        b (.getBody (.shape2 cp))
        aname (body-userdata a :name)
        bname (body-userdata b :name)]
    ;; bullet-player damage
    (cond (or (= aname 'player-1) (= aname 'player-2)) (player-contact state a bname b)
          (or (= bname 'player-1) (= bname 'player-2)) (player-contact state b aname a))))

(defn set-contact-listener [world state]
  (let [cl (proxy [ContactListener] []
             (add [#^ContactPoint cp]
               (comment state :pain)
               (contact-add cp state))
             (persist [_])
             (remove [_])
             (result [_]))]
    (.setContactListener world cl)))

(comment (with-world (set-contact-listener world state)))

(defn show-damage []
  (let [[p1 p2] (with-world [(.get (.get state (.get state 'player-1)) 'damage)
                             (.get (.get state (.get state 'player-2)) 'damage)])]
    [p1 p2]))

(comment (show-damage))

;; destroy-player animation

(defn create-player-debris [#^World world player]
  ;; create new bodies representing the players wreck debris :)
  (let [pos (.getPosition player)
        v (.getLinearVelocity player)
        a (.getAngularVelocity player)
        mkbody #(doto (make-simple-box world 'debris
                                       :pos [(+ (.x pos) (+ 0.1 (rand 0.1)))
                                             (+ (.y pos) (+ 0.1 (rand 0.1)))]
                                       :shape [(+ (rand 0.3) 0.05) (+ (rand 0.3) 0.05)]
                                       :linear-damping 1
                                       :angular-damping 1)
                  (.setLinearVelocity (.mul v (rand 1.4)))
                  (.setAngularVelocity (* a (rand 1.5))))]
    (dotimes [n 26] (mkbody))))

(comment (with-world (create-player-debris world (.get state 'player-1))))

;; player-gc: destroy a damaged player
(def max-player-hitpoints 2)

(defn player-destroyed [player-name]
  (hoeck.thread/thread-sleep 3000)
  (restart-game))

(declare game-invoke-later)
(defn destroy-player-job [#^World world tick #^HashMap state]
  (let [hp max-player-hitpoints ;; <- hitpoints
        p (.get state 'player-1)
        o (.get state 'player-2)
        p-data (.get state p)
        o-data (.get state o)]
    (when (and (< hp (or (.get p-data 'damage) 0)) (not (get p-data 'dead)))
      (.destroyBody world p)
      (create-player-debris world p)
      (.put p-data 'dead true)
      (game-invoke-later #(player-destroyed 'player-1)))
    (when (and (< hp (or (.get o-data 'damage) 0)) (not (get o-data 'dead)))
      (.destroyBody world o)
      (create-player-debris world o)
      (.put o-data 'dead true)
      (game-invoke-later #(player-destroyed 'player-2)))))

(comment (add-job 'destroy-player destroy-player-job))
(comment (show-damage))

(def current-player-hitpoints (atom [0 0]))

(defn update-player-hitpoints-job [world tick #^HashMap state]
  (when (= (rem tick 3) 0)
    (let [p1 (or (.get (.get state (.get state 'player-1)) 'damage) 0)
          p2 (or (.get (.get state (.get state 'player-2)) 'damage) 0)]
      (swap! current-player-hitpoints (constantly [p1 p2])))))


;; extract all body state for syncing with our peer player

;; only a known set of bodies will be created during the game (player, planet, bullets)
;; so its enough to only sync pos, angle, linear velocity, angular velocity
;; the final explosion may differ on both worlds

(def current-body-state (atom {:tick 0 :state []}))

(defn extract-body-state [#^World world tick state]
  (when (zero? (rem tick 10))
    (let [bstate (iter (for #^Body b call .getNext on (.getBodyList world))
                       (collect [(body-userdata b :name)
                                 (vec2->clj (.getPosition b))
                                 (.getAngle b)
                                 (vec2->clj (.getLinearVelocity b))
                                 (.getAngularVelocity b)]))]
      (swap! current-body-state (constantly {:tick tick :state bstate})))))

(comment (with-world (extract-body-state world 10 state)))      

;; executing game logic

(def game-thread-queue (java.util.concurrent.LinkedBlockingQueue.)) ;; stuff executed on the game logic thread

(defn game-invoke-later [f]
  (.put game-thread-queue f))

(defn game-exec []
  (while true
    (try ((.take game-thread-queue))
         (catch Exception e (println "Exception" e "in game-exec thread")))))

;; setup

(defn setup-game []

  (init {:gravity [0 0]})

  (restart-game)

  ;; setting camera
  (set-my-camera-zoom 5)
  ;;(set-pos 150 150) -> set by window-resized
  
  ;; adding the draw function
  (redef draw
    (my-draw @world-state))

  ;; add job: observing jbox objects
  (add-job 'write-state write-world-state)

  ;; add job: central gravity
  (add-job 'gravity apply-gravity)

  ;; add job: stepping the jbox engine
  ;; this actually starts the simulation
  (add-job 'step step-world)

  ;; the player control
  (add-job 'player-control player-control)

  ;; remove bullets after some time
  (add-job 'bullet-gc bullet-gc)

  ;; make objects wrap at the worlds borders
  (add-job 'wrap-world wrap-world-job)
  
  ;; contact listener to apply damage
  (with-world (set-contact-listener world state))

  ;; destroying one another
  (add-job 'destroy-player destroy-player-job)

  ;; hitpoints
  (add-job 'update-player-hitpoints update-player-hitpoints-job)
  
  ;; getting sync data
  (add-job 'extract-body-state extract-body-state)
  
  ;; setup the game-logic
  
  )


(defn restart-game []
  (with-world (clear-world world)) ;; remove all objects form world
  ;; add some test objects
  (with-world
    (let [;;player-1 (make-simple-box world 'player-1 :shape [0.7 1] :pos [0 +15] :angular-damping 2.0 :linear-damping 0)
          ;;player-2 (make-simple-box world 'player-2 :shape [0.7 1] :pos [0 -15] :angular-damping 2.0 :linear-damping 0)
          player-1 (make-player-body world 'player-1 player-1-draw
                                     {:pos [0 +14] :angular-damping 2.0 :linear-damping 0})
          player-2 (make-player-body world 'player-2 player-2-draw
                                     {:pos [0 -14] :angular-damping 2.0 :linear-damping 0})
          planet   (make-simple-circle world 'planet :radius 5 :dynamic false :friction 0.03 :draw-fn planet-draw)]
      
      ;; store
      (.put state 'player-1 player-1)
      (.put state player-1 (HashMap.)) ;; player-1 data, like bullet count

      (.put state 'player-2 player-2)
      (.put state player-2 (HashMap.)) ;; player 2 data

      (.put state 'planet planet)

      ;; initial thrust
      (.setLinearVelocity player-1 (vec2 [19 0]))
      (.setLinearVelocity player-2 (vec2 [-19 0])))))


(redef key-typed 
  (when (:o @hoeck.clojurebox2d.processing/keystatus)
    (game-invoke-later restart-game)))

(do
  (game-invoke-later setup-game)
  (game-invoke-later restart-game))

(game-exec)

;; game thread:
;; mode: client/server
;; wait until-tick: game start (add-job step)
;; syncronize: client: wait for 
;; 


