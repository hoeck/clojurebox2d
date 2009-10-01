
(ns hoeck.clojurebox2d.example
  (:use hoeck.clojurebox2d
        ;; clojurebox libs
        hoeck.clojurebox2d.jbox2d
        hoeck.clojurebox2d.utils
        hoeck.clojurebox2d.processing
        hoeck.clojurebox2d.processing-utils

        ;; example libs
        hoeck.clojurebox2d.example.player
        hoeck.clojurebox2d.example.stars
        hoeck.clojurebox2d.example.colors
        
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
  [world-state]
  (no-smooth)
  ;; camera transformations
  (let [[offset, zoom] @my-camera]
    (translate offset)
    (scale zoom))

  (memoized-background draw-background-stars applet-resized?)

  ;; drawing bodies
  (doseq [[shape-data {:keys [draw-fn]}] world-state]
    (if draw-fn (draw-fn shape-data))))

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

;; setup
(comment
  (defn restart-game []
    ;; add some test objects
    (with-jbox2d
      (clear-world *world*);; remove all objects from world
      (let [state *state*
            player-red (make-player-body *world* 'player-1 player-1-draw
                                         {:pos [0 +14] :angular-damping 2.0 :linear-damping 0 :angle 0})
            player-blue (make-player-body *world* 'player-2 player-2-draw
                                          {:pos [0 -14] :angular-damping 2.0 :linear-damping 0})
            planet   (make-circle *world* 'planet :radius 5 :dynamic false :friction 0.03 :draw-fn planet-draw :angle PI)]
      
        ;; store
        (.put state 'player-red player-red)
        (.put state player-red (HashMap.));; player-1 data, like bullet count

        (.put state 'player-blue player-blue)
        (.put state player-blue (HashMap.));; player 2 data

        (.put state 'planet planet)

        ;; initial thrust
        (.setLinearVelocity player-red (vec2 [19 0]))
        (.setLinearVelocity player-blue (vec2 [-19 0]))))))


;; no title

(defn setup-world [] 
  
  (init {:gravity [0 0] :smooth false :setup-hook color-setup})

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

(defn initialize []
  (clear-agent-errors game)  

  ;; setup game state
  (send game 
        (fn [_] 
          (-> {}
              (merge (setup-world))
              (assoc :player (make-player 1 :red)))))
  
  (await game)
  (redef draw [] (my-draw @world-state))

  (send game 
        (fn [G]
          ;;(generate-stars (:wrap-world-box %) [10 4] game)
          ;; enable player control
          (swap! (-> G :player-control) 
                 (constantly (fn [tick] (player-control (-> @game :player :state)))))
          ;; rescurrect player
          ((-> G :player :state) :reset (-> G :player :name))
          ((-> G :player :state) :alive)
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


(defn reset-player []
  ((or (-> @game :player :state) (fn [_])) :destroy)
  (dorun (map #(send game %)
              [#(assoc % :player (make-player 1 :red))
               #(do (swap! (-> % :player-control) (constantly (fn [tick] (player-control (-> @game :player :state))))) %)
               #(do ((-> % :player :state) :reset) %)
               #(do ((-> % :player :state) :alive) %)])))


;(defmethod contact [:bullet :player] [b0 b1 t cp]
;  (when (= t :add)
;    (add-event )
;    (println (format "player %s hit by bullet %s" b1 b0))
;    ))

;; (with-jbox2d (register-contact-multimethod *world*))

(comment

  ;;(with-jbox2d (:draw-fn (first (map #(-> % .getBody body-userdata) (seq (query *world*))))))
  ;;(show-sketch! (fn [] (with-jbox2d (:draw-fn (first (map #(-> % .getBody body-userdata) (seq (query *world*))))) [[0 0] [-1 1] [1 -1]])))

  ((-> @game :player :state))
  ((-> @game :player :state) :reset)
  ((-> @game :player :state) :alive)
  ((-> @game :player :state) :move :left)
  ((-> @game :player :state) :move :right)
  (dotimes [n 1000] ((-> @game :player :state) :move :thrust))
  ((-> @game :player :state) :destroy)
  (with-jbox2d (.destroyBody *world* (.get *state* (-> @game :player :name))))
  (with-jbox2d *state*)

  (with-jbox2d (destroy-body *world* (.get *state* (-> @game :player :name))))

  (with-jbox2d (dotimes [n 100] (make-circle 'foo {})))
  
  (with-jbox2d (body-userdata (.get *state* 1) :foo))
(with-jbox2d (dorun (map #(body-userdata %) (query *world*))))   
(with-jbox2d (iter (for shape in-array (query *world*))
                   (for body as (.getBody shape))
                   (for userdata as (body-userdata body))
                   (collect userdata)))
)

(comment
  (setup-game)

  (with-jbox2d (clear-world *world*))
  (with-jbox2d (dotimes [n 40] (make-star [39 30])))
  (with-jbox2d
    ;; test shortlived objects
    (dotimes [n 100]
      (let [b (make-circle 'foo {:pos [10, 10]})]
        (add-event* (* n 30) (fn [tick] (.destroyBody *world* b)))))
    
    )
  (with-jbox2d
    (dotimes [n 2]
      (make-box 'foo {:pos [10, 10] :shape [1 1]}))
    
    )
  
  (with-applet (my-draw @world-state))

  (with-jbox2d (make-circle 'planet-2
                            {:pos [0 0]
                             :radius 10
                             :dynamic false
                             :friction 0.03
                             :draw-fn planet-draw
                             :angle PI}))

  (with-jbox2d
    (make-circle 'planet-1
                 {:pos [-30 -20]
                  :radius 5
                  :dynamic false
                  :friction 0.03
                  :draw-fn planet-draw
                  :angle PI})
    (let [b
          (make-circle 'planet-2
                       {:pos [30 20]
                        :radius 5
                        :dynamic false
                        :friction 0.03
                        :draw-fn planet-draw
                        :angle PI})]

      (.getWorldCenter b)))

  )




;; game thread:
;; mode: client/server
;; wait until-tick: game start (add-job step)
;; syncronize: client: wait for 
;; UDP! -> DatagramSocket


