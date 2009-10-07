
;; generate random background stars
(ns hoeck.clojurebox2d.example.stars
  (:use hoeck.clojurebox2d
        hoeck.clojurebox2d.jbox2d
        hoeck.clojurebox2d.utils
        hoeck.clojurebox2d.processing
        hoeck.clojurebox2d.processing.utils
        hoeck.clojurebox2d.processing.colors
        hoeck.iterate
        rosado.processing))

(defn star
  "Create a function which draws a random star polygon 
  at with a given color. Return a map of :draw-fn and :star,
  which contains the given star parameters.
  Without any args, choose parameters randomly."
  ([] (let [radius [4 11]
            waist [0.2 0.7]
            corners [5 7]
            fill-alphas (range 0 30 10)
            stroke-alphas (range 30 50 5)
            stroke-weights [1 1 2]
            r (apply rrand radius)
            a (rrand (- HALF_PI) HALF_PI)
            w (apply rrand waist)
            c (apply rrand corners)
            ;;fc (rand-nth fill-colors)
            fa (rand-nth fill-alphas)
            sa (rand-nth stroke-alphas)
            sw (rand-nth stroke-weights)]
        (star r (* r w) c a fa sa sw)))
  ([radius inner-radius corners angle fill-alpha stroke-alpha stroke-weight-param]
     (let [p (map #(v2-rotate angle %) (star-points radius inner-radius corners))]
       {:draw-fn (fn [color]
                   (stroke-weight stroke-weight-param)
                   (stroke-int color stroke-alpha)
                   (fill-int color fill-alpha)
                   (begin-shape)
                   (doseq [[vx vy] p]
                     (vertex vx vy))
                   (let [[x y] (first p)] (vertex x y))
                   (end-shape))
        :star [radius inner-radius corners angle fill-alpha stroke-alpha stroke-weight-param]})))

(defn make-star
  "create a star body in *world* at initial-position."
  [initial-position]
  (let [s (star)
        df (:draw-fn s)
        color (color :grey)]
    (make-body *world*
               (make-body-userdata :name 'star
                                   :draw-fn (fn [[p r]]
                                              (with-translation p
                                                (df color)))
                                   :star (:star s)
                                   :color color)
               {:shape (first (:star s))
                :pos initial-position
                :dynamic true
                :angular-damping 0.0
                :linear-damping 0.0
                :friction 0.3})))

(defn random-star-positions
  "Generate a randomized points in the box p0 - p1, with xn by yn points."
  [[x0 y0 x1 y1] [xn yn]]
  (map #(v2+ % (v2* (rrand 0.5 10) (rand-circle-point)))
       (grid-points [x0 y0] [x1 y1] [xn yn])))

(defn make-starfield [positions]
  (into [] (map make-star positions)))

(defn catch-stars [world]
  (iter (for b in-array (.getBodyList world))
        (with stars [])
        (collect-if (= (body-userdata b :name) 'star) b)))

(defmacro do1
  "like do but return the result from the first form."
  [& body]
  `(let [result# ~(first body)]           
    ~@(next body)                         
    result#))

(defn freeze-stars [world stars]
  (map #(do1 {:pos (vec2->clj (.getWorldCenter %))
              :star (:star (body-userdata %))} ;;(-> % body-userdata :star-data :star)
             (.destroyBody world %))
       stars))

(defn generate-stars
  "generate a field of stars, return a list of functions to draw each one.
  (star-fn color) -> changes color
  (star-fn) -> draws the star."
  [wrap-world-box [xn yn] game-agent]
  (let [initial-positions (random-star-positions wrap-world-box [xn yn])
        star-bodies (with-jbox2d (make-starfield initial-positions))]
    (add-event 20
      (let [s (freeze-stars *world* star-bodies)
            s-draw-fns (doall (map #(let [p (:pos %)
                                          df (:draw-fn (apply star (:star %)))
                                          c (atom -8355712)]
                                      (fn star-draw-fn
                                        ([color] (swap! c (constantly color)))
                                        ([] (with-translation p
                                              (df @c)))))
                                   s))]
        (println "assoc'ing:" game-agent :stars s-draw-fns) 
        (send game-agent assoc :stars s-draw-fns)))))


