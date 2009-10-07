
;; player related code
(ns hoeck.clojurebox2d.example.player
  (:use hoeck.clojurebox2d
        hoeck.clojurebox2d.statemachine
        hoeck.clojurebox2d.jbox2d
        hoeck.clojurebox2d.utils
        hoeck.clojurebox2d.processing
        hoeck.clojurebox2d.processing.utils
        hoeck.clojurebox2d.processing.colors
        hoeck.iterate
        rosado.processing)
  (:import (java.util HashMap LinkedList ArrayDeque)
           (org.jbox2d.collision.shapes Shape)
           (org.jbox2d.dynamics Body World ContactListener)
           (org.jbox2d.dynamics.contacts ContactPoint)))

(def constants
     {:hitpoints 10
      :bullets 10
      :thrust-amount 110
      :brake-amount -70
      :turn-amount 20})

;; jbox2d

(defn make-player-body [userdata body-def-args]
  (make-body *world*
             userdata
             (merge {:dynamic true
                     :shape [[-1.1 -1.5] [1.1 -1.5] [0 1.1]]
                     :angular-damping 1.0
                     :linear-damping 0.0}
                    body-def-args)))

;; processing

(defn make-player-draw-fn [color-int]
  (let [black (color :black 0)]
    (fn player-draw [[[x0 y0] [x1 y1] [x2 y2]]]
      (stroke-weight 1)
      (stroke-int black)
      (fill-int color-int)
      (triangle x0 y0 x1 y1 x2 y2))))

;; player-control

(defn turn-body [#^Body player left-or-right amount]
  (when player
    (.applyTorque player
                  (if (= left-or-right :left)
                    (* -1 amount)
                    amount))))

(defn thrust-body [#^Body player amount]
  (when player
    (.applyForce player
                 (.mul (.getWorldVector player (vec2 [0 1])) amount)
                 (.getWorldCenter player))))

(defmacro keyaction
  "Expands into (when ..) expressions calling the state-machine with
  :move MOVECMD messages when a given KEY in the given keystatusset is true.
  if MOVECMD is a vector, then omit the :move msgkey."
  [keystatus statemachine & key-movecmd-pairs]
  (let [sm_ (gensym 'statemachine)
        ks_ (gensym 'keystatus)]
    `(let [~sm_ ~statemachine
           ~ks_ ~keystatus]
       (when ~sm_
         ~@(map (fn [[key movecmd]] `(when (~key ~ks_)
                                       ~(if (keyword? movecmd)
                                          `(~sm_ :move ~movecmd)
                                          `(~sm_ ~@movecmd))))
                (partition 2 key-movecmd-pairs))))))

(defn player-control [player-state-machine]
  (let [ks @keystatus
        p player-state-machine]
    (keyaction ks player-state-machine
               :x [:kill]
               :c [:reset]
               :v [:alive]
               :up :thrust
               :down :brake
               :left :left
               :right :right
               :space :shoot)))

(defn two-player-control [player-1-state-machine player-2-state-machine]
  (let [ks @keystatus]
    (keyaction ks player-1-state-machine
               :up :thrust
               :down :brake
               :left :left
               :right :right
               :space :shoot)
    (keyaction ks player-2-state-machine
               :w :thrust
               :s :bracke
               :a :left
               :d :right
               :q :shoot)))

;; bullets

(defn bullet-draw [[[x y] r]]
  (stroke-int (color :black))
  (fill-int (color :white))
  (let [d (* 2 r)]
    (ellipse x y d d)))

(defn make-bullet
  "make a bullet and store some methods in its userdata.
  I wish I had newnew for this"
  [player player-state-machine]
  (let [tick @current-tick
        player-name (body-userdata player :name)
        bullet-pos (.getWorldPoint player (vec2 0 2.0));; somewhere in front of the spacecraft
        #^Body bullet (make-body *world*
                                 ;; generate a unique bullet name to identify objects across the network line
                                 (make-body-userdata :name (str 'bullet- player-name "-" tick)
                                                     :draw-fn bullet-draw
                                                     :type :bullet)
                                 {:pos (vec2->clj bullet-pos)
                                  :shape 0.6
                                  :linear-damping 1.5})
        p-pos (.getPosition player)
        destroy-bullet (fn destroy-bullet []
                         (when (not (body-userdata bullet :destroyed))
                           (destroy-body *world* bullet)
                           (player-state-machine :bullet-dec)))]

    ;; initial bullet speed        
    (.setLinearVelocity bullet (.getLinearVelocity player))
    (.applyImpulse bullet (.getWorldVector player (vec2 [0 120])) bullet-pos)

    (alter-body-userdata bullet assoc :destroy-method destroy-bullet)

    ;; return the destructor
    destroy-bullet))

(defn player-shoot [#^Body player player-state-machine]
  (when player
    (let [b (make-bullet player player-state-machine)]
      (add-event 150 (b)))))

;; model player with a state machine:
(def-state-machine player-state)

(defsmethod player-state :init [player-name player-color]  
  (add-event 30 (current-state-machine :reset))
  {:state :init
   :draw-fn (make-player-draw-fn (color player-color))
   :player-name player-name})

(defsmethod player-state :reset [:player-name :draw-fn]
  (add-event 0 (when (not (.get *state* player-name))
                 (let [b (make-player-body
                          (make-body-userdata :name player-name
                                              :draw-fn draw-fn
                                              :type :player)
                          {:pos [(rrand -50 50) (rrand -30 30)]})]
                   (.put *state* player-name b))))
  (add-event 30 (current-state-machine :alive))
  {:state :init
   :hp (constants :hitpoints)
   :bullets 0
   :last-shot 0})

(defsmethod player-state :alive []
  {:state :alive})

(defsmethod player-state :hit [:hp :state hp-amount]
  (when (= state :alive)
    (let [hp (- hp hp-amount)]
      (if (< 0 hp) 
        {:hp hp}
        (do (current-state-machine :kill) {:hp 0})))))

(defsmethod player-state :move [:state :bullets :last-shot :player-name move-cmd]
  (when (= state :alive)
    (condp = move-cmd
      :left   (do (add-event 0 (turn-body    (.get *state* player-name) :left  (:turn-amount constants))) nil)
      :right  (do (add-event 0 (turn-body    (.get *state* player-name) :right (:turn-amount constants))) nil)
      :thrust (do (add-event 0 (thrust-body  (.get *state* player-name) (:thrust-amount constants))) nil)
      :shoot  (when (and (< bullets (:bullets constants)) (< 6 (- @current-tick last-shot)))
                (add-event 0 (player-shoot (.get *state* player-name) current-state-machine))
                {:bullets (inc bullets) :last-shot @current-tick})
      :brake  (do (add-event 0 (thrust-body  (.get *state* player-name) (:brake-amount constants))) nil))))

(defsmethod player-state :kill [:state :player-name]
  (when (= state :alive)
  ;; destruction animation
    (add-event 0
      (destroy-body *world* (.get *state* player-name))
      (make-explosion-1 (.get *state* player-name) 3 40 (rrand 1 3))
      (.remove *state* player-name))
    (add-event 120 (current-state-machine :reset)) ;; respawn
    {:state :killed
     :hp 0}))

(defsmethod player-state :bullet-dec [:bullets]
  {:bullets (dec bullets)})

;; player ctor
(defn make-player
  "returns state machine fn."
  [name player-color]
  (make-state-machine player-state :init name player-color))

;; player-getters

(defn player-get-vitality
  "return a number in [0,1], 0..dead, 1..alive"
  [player-sm]
  (when player-sm
    (/ (or (@(player-sm) :hp) 0) (:hitpoints constants))))