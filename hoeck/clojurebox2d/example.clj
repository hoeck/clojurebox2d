
(ns hoeck.clojurebox2d.example
  (:use hoeck.clojurebox2d
        ;; clojurebox libs
        hoeck.clojurebox2d.jbox2d
        hoeck.clojurebox2d.utils
        hoeck.clojurebox2d.processing
        hoeck.clojurebox2d.processing.utils
        hoeck.clojurebox2d.processing.colors
        hoeck.clojurebox2d.processing.widgets

        ;; example libs
        hoeck.clojurebox2d.example.player
        hoeck.clojurebox2d.example.stars
        
        ;; additional libs
        hoeck.iterate
        rosado.processing
        clojure.contrib.pprint
        clojure.contrib.def
        hoeck.thread)
  (:import (java.util HashMap LinkedList ArrayDeque)
           (org.jbox2d.collision.shapes Shape)
           (org.jbox2d.dynamics Body World ContactListener)
           (org.jbox2d.dynamics.contacts ContactPoint)))

;(ns-unmap *ns* 'defn)
;(defmacro defn [& rest]
;  `(do (println "defining:" (first '~rest))
;       (clojure.core/defn ~@rest)))

(defn throw-agent-error [a]
  (when-let [ae (first (agent-errors a))]
    (throw ae)))

(declare game)

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
              (get-circle-points shape)
              ;; more to come: polyshape, multi-shaped-bodies
              )
            (body-userdata body)
            ;(body-userdata body :name)
            ;(body-userdata body :draw-fn)
            )))

(def world-state (atom []))

(defn write-world-state [tick]
  (if (even? tick) ;; every 2 ticks
    (let [clojure-body-definitions (iter (for shape in-array (query *world*))                                         (collect (jbox->clojurebox shape)))]
      (swap! world-state (constantly clojure-body-definitions)))))

;; draw

(defn default-style []
  (stroke-float (color :black))
  (fill-int (color :white)))

(defn box-draw [[[x0 y0] [x1 y1] [x2 y2] [x3 y3]]]
  (fill 0)
  (stroke-float 0)
  (quad x0 y0
        x1 y1
        x2 y2
        x3 y3))

(defn circle-draw [[[x y] r]]
  (stroke-int (color :black))
  (fill-int (color :white))
  (let [d (* 2 r)]
    (ellipse x y d d)))

(defn planet-draw [[[x y] r]]
  (stroke-int (color :black))
  (stroke-weight 1)
  (fill-int (color :white))
  (let [d (* 2.2 r)]
    (ellipse x y d d)))

(defn draw-background-stars []
  (background-int (color :white))
  (doseq [s (@game :stars)] 
    (s)))

(defn my-draw
  "Draws all body shapes (only boxes for now) using quad."
  ([world-state] (my-draw world-state nil))
  ([world-state osd-draw-fn]
  ;;(no-smooth)

  ;; camera transformations
     (let [[offset, zoom] @my-camera]
       (with-translation offset
         (scale zoom)

         (memoized-background draw-background-stars applet-resized?)
    
         ;; drawing bodies
         (doseq [[shape-data {:keys [draw-fn]}] world-state]
           (if draw-fn (draw-fn shape-data)))))
     (scale 1)
     (when osd-draw-fn (osd-draw-fn))))

;; center the current *world* on resize
;; adjust zoom so that the whole *world* fits in the frame
(redef window-resized []
  (when (and (:frame cbox) (:wrap-world-box @game))
    (let [[x0 y0 x1 y1] (:wrap-world-box @game)
          world-width (+ (Math/abs x0) x1)
          world-height (+ (Math/abs y0) y1)
          dimension (.getSize #^javax.swing.JFrame (:frame cbox))
          width (. dimension width)
          height (. dimension height)
          zoom (min (/ width world-width)
                    (/ height world-height))]
      (set-my-camera-pos (int (/ width 2))
                         (int (/ height 2)))
      (set-my-camera-zoom zoom))))

;; bodies

(defn bodies-info []
  (with-jbox2d 
    (map #(body-userdata #^Body %)
         (set (map #(.getBody #^Shape %)
                   (query *world*))))))

(defn make-box [name args]
  (make-body *world* 
             (make-body-userdata :name name :draw-fn box-draw)
             (merge {:shape [1 1]
                     :pos [0 0]}
                    args)))

(defn make-circle
  [body-name argmap]
  (make-body *world* 
             (make-body-userdata :name body-name 
                                 :draw-fn (:draw-fn argmap circle-draw))
             (merge {:shape (:radius argmap 1)
                     :pos [0 0]
                     :dynamic true
                     :angular-damping 0.0
                     :linear-damping 0.0
                     :friction 0.3
                     :draw-fn circle-draw}
                    argmap)))

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


;; game state

(def game (agent {}))

(defn player
  "accessor for the player statemachines."
  ([] (when game (@game :player)))
  ([player-name & args]
     (when (and game @game)
       (if args
         (apply ((@game :player) player-name) args)
         ((@game :player) player-name)))))


;; On Screen Display

(defn make-osd-draw-fn []
  (let [p1-vit (make-levelmeter :a-data-fn #(- 1 (player-get-vitality (player 1)))
                                :color-a (color :white)
                                :color-b (color :red)
                                :stroke-color (color :black)
                                :size [50 10]
                                :direction :horizontal)
        p2-vit (make-levelmeter :a-data-fn #(player-get-vitality (player 2))
                                :color-a (color :blue)
                                :color-b (color :white)
                                :stroke-color (color :black)
                                :size [50 10]
                                :direction :horizontal)]  
    (fn draw-osd []
      (when (player 1) (place-widget p1-vit :right :bottom 5))
      (when (player 2) (place-widget p2-vit :left :bottom 5)))))


;; clojurebox2d setup

(defn setup-world []
  
  (init {:gravity [0 0] :smooth false :setup-hook color-setup})
  (write-tick 0)

  (let [gravity (atom 0)
        player-c (atom (fn [_]))
        worldsize [-70 -50 70 50]]
    (reset-events)    

    (redef jbox2d-loop [tick]
      (write-tick tick)
      (run-timed-events tick);; timed events created with add-event, with-jbox2d, schedule-event
      (central-gravity [0 0], @gravity)
      
      (@player-c tick)
      
      (step-world tick)
      (wrap-world worldsize)
      (at-tick 2 0 (write-world-state tick)) ;; write world for drawing
      )
    
    (with-jbox2d (register-contact-multimethod *world*))

    {:gravity gravity
     :wrap-world-box worldsize
     :player-control player-c}))


;; game setup

(defn initialize
  "Set up clojurebox2d and the game state"
  []
  (clear-agent-errors game)  
  (redef draw [] (my-draw []))

  ;; setup game state
  (send game 
        (fn [_] 
          (-> {}
              (merge (setup-world))
              (assoc-in [:player 1] (make-player 1 :red))
              (assoc-in [:player 2] (make-player 2 :blue)))))

  (await game)
  (redef draw [] (my-draw @world-state (make-osd-draw-fn)))

  (send game
        (fn [G]
          ;;(generate-stars (:wrap-world-box %) [10 4] game)
          ;; enable player control
          (swap! (G :player-control) 
                 (constantly (fn [tick] (two-player-control (player 1) (player 2)))))
          ;; rescurrect player
          ;;((G :player) :reset)
          ;;((G :player) :alive)
          ;; planet body           
          (add-event 0 (make-body *world*
                                  (make-body-userdata :name 'planet 
                                                      :type :planet
                                                      :draw-fn planet-draw)
                                  {:pos [0 0]
                                   :shape 5
                                   :dynamic false
                                   :friction 0.00
                                   :draw-fn planet-draw
                                   :angle PI}))
          ;; apply gravity
          (-> G :gravity (swap! (constantly 15000)))
          ;; do not change the agent
          G)))

;; let weapons damage the player
(defmethod contact [:bullet :player] [b0 b1 t cp]
  (when (= t :add)
    ;; destroy the bullet
    (add-event 0 ((body-userdata b0 :destroy-method)))
    (player (body-userdata b1 :name) :hit 1)))

;; debris is destroyed on planet contact
(defmethod contact [:debris :planet] [b0 b1 t cp]
  (add-event 0 (destroy-body *world* b0)))

;; debris add small amount of damage to the player
(defmethod contact [:debris :player] [b0 b1 t cp]
  (player (body-userdata b1 :name) :hit 0.1))

;; game thread:
;; mode: client/server
;; wait until-tick: game start (add-job step)
;; syncronize: client: wait for 
;; UDP! -> DatagramSocket


