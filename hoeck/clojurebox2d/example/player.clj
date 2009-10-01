
;; player related code
(ns hoeck.clojurebox2d.example.player
  (:use hoeck.clojurebox2d
        hoeck.clojurebox2d.statemachine
        hoeck.clojurebox2d.jbox2d
        hoeck.clojurebox2d.utils
        hoeck.clojurebox2d.processing
        hoeck.clojurebox2d.processing-utils
        hoeck.clojurebox2d.example.colors
        hoeck.iterate
        rosado.processing)
  (:import (java.util HashMap LinkedList ArrayDeque)
           (org.jbox2d.collision.shapes Shape)
           (org.jbox2d.dynamics Body World ContactListener)
           (org.jbox2d.dynamics.contacts ContactPoint)))

(def constants
     {:hitpoints 10
      :bullets 5
      :thrust-amount 110
      :brake-amount -70
      :turn-amount 20})

;; model player with a state machine:
(declare turn-body, thrust-body, player-shoot)

(def-state-machine player-state)

(defsmethod player-state :reset [player-name]
  {:state :init
   :hp (constants :hitpoints)
   :bullets 0
   :last-shot 0
   :player-name player-name})

(defsmethod player-state :alive []
  {:state :alive})

(defsmethod player-state :hit [:hp :state hp-amount]
  (when (= state :alive)
    (let [hp (- hp hp-amount)]
      (if (< 0 hp) 
        {:hp hp}
        (do (state-machine :killed) nil)))))

(defsmethod player-state :move [:state :bullets :last-shot :player-name move-cmd]
  (when (= state :alive)
    (condp = move-cmd
      :left   (do (add-event 0 (turn-body    (.get *state* player-name) :left  (:turn-amount constants))) nil)
      :right  (do (add-event 0 (turn-body    (.get *state* player-name) :right (:turn-amount constants))) nil)
      :thrust (do (add-event 0 (thrust-body  (.get *state* player-name) (:thrust-amount constants))) nil)
      :shoot  (when (and (< bullets (:bullets constants)) (< 6 (- @current-tick last-shot)))
                (add-event 0 (player-shoot (.get *state* player-name) state-machine))
                {:bullets (inc bullets) :last-shot @current-tick})
      :brake  (do (add-event 0 (thrust-body  (.get *state* player-name) (:brake-amount constants))) nil))))

(defsmethod player-state :kill [:player-name]
  ;; destruction animation
  (add-event 0 (destroy-body *world* (.get *state* player-name)))
  {:state :killed})

(defsmethod player-state :bullet-dec [:bullets]
  {:bullets (dec bullets)})

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

;; player ctor  
;;(def player-state-transitions (atom []))
(defn make-player
  "returns a hashmap:
  :name  .. id, as a key in the world state hashmap
  :color .. players color keyword
  :state .. state machine closure"
  [name player-color]
  (let [pdraw (make-player-draw-fn (color player-color))
        player-state (make-state-machine player-state :reset name)
        ;;player-state (make-debugging-state-machine player-state-transitions name :reset name)
        ]
    (add-event 0 (let [b (make-player-body
                          (make-body-userdata :name name
                                              :draw-fn pdraw
                                              :type :player)
                          {})]
                   (.put *state* name b)))
    {:color player-color
     :name name
     :state player-state}))

;; player-control

(defn turn-body [#^Body player left-or-right amount]
  (.applyTorque player
                (if (= left-or-right :left)
                  (* -1 amount)
                  amount)))

(defn thrust-body [#^Body player amount]
  (.applyForce player
               (.mul (.getWorldVector player (vec2 [0 1])) amount)
               (.getWorldCenter player)))

(defn player-control [player-state-machine]
  (let [ks @keystatus
        p player-state-machine]
    (when p
      (when (:up ks)    (p :move :thrust))
      (when (:down ks)  (p :move :brake))
      (when (:left ks)  (p :move :left))
      (when (:right ks) (p :move :right))
      (when (:space ks) (p :move :shoot)))))

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
        p-pos (.getPosition player)]

    ;; initial bullet speed        
    (.setLinearVelocity bullet (.getLinearVelocity player))
    (.applyImpulse bullet (.getWorldVector player (vec2 [0 120])) bullet-pos)

    ;; methods
    (fn destroy-bullet [] 
      (when (not (body-userdata bullet :destroyed))
        (destroy-body *world* bullet)
        (player-state-machine :bullet-dec)))))

(defn player-shoot [#^Body player player-state-machine]
  (let [b (make-bullet player player-state-machine)]
    (add-event 120 (b))))


(comment (with-jbox2d (player-shoot (.get *state* 1))))

;; destroy-player animation

;;(defn create-player-debris [player]
;;  ;; create new bodies representing the players wreck debris :)
;;  (let [pos (.getPosition player)
;;        v (.getLinearVelocity player)
;;        a (.getAngularVelocity player)
;;        mkbody #(doto (make-box 'debris
;;                                :pos [(+ (.x pos) (+ 0.1 (rand 0.1)))
;;                                      (+ (.y pos) (+ 0.1 (rand 0.1)))]
;;                                :shape [(+ (rand 0.3) 0.05) (+ (rand 0.3) 0.05)]
;;                                :linear-damping 1
;;                                :angular-damping 1)
;;                  (.setLinearVelocity (.mul v (rand 1.4)))
;;                  (.setAngularVelocity (* a (rand 1.5))))]
;;    (dotimes [n 26] (mkbody))))


