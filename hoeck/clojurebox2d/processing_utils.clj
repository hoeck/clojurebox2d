
;; "primitives for 2d drawing & some vector math"
(ns hoeck.clojurebox2d.processing-utils
  (:use hoeck.clojurebox2d.processing
        hoeck.iterate
        rosado.processing)
  (:import (java.util.concurrent ArrayBlockingQueue)))

;; vector sequences

(defn circle-points
  "Returns n points from a circle with radius, starting at the initial angle
  (in radians) r0."
  [radius n r0]
  (map #(vector (* radius (cos %)) (* radius (sin %)))
       (range r0 (+ (* 2 PI) r0) (* 2 (/ PI (int n))))))

(defn grid-points
  "Return a grid of points in a box defined by v0, v1.
  [x0 y0]: lower bound vector
  [x1 y1]: upper bound vector
  [xn yn]: number of points along the x/y axis"
  [[x0 y0] [x1 y1] [xn yn]]
  (let [grid (fn [a b n] (range (+ (/ (Math/abs (- a b)) n 2) a) b (/ (Math/abs (- a b)) n)))
        grid-x (grid x0 x1 xn)
        grid-y (grid y0 y1 yn)]
    (for [y grid-y x grid-x] [x y])))

(defn star-points
  "Generate a list of points used to draw a star."
  ([radius inner-radius corners]
     (star-points radius inner-radius corners 0))
  ([radius inner-radius corners radial-offset]
     (interleave (circle-points radius corners 0)
                 (circle-points inner-radius corners (+ (/ PI corners) radial-offset)))))

(defn line-points
  "Returns a seq of steps points on the line beginning at p0 to p1."
  [[x0 y0] [x1 y1] steps]
  (let [;; y = mx + n
        m (/ (- y0 y1) (- x0 x1)) ;; slope
        n (- y0 (* m x0)) ;; shift
        f #(vector % (+ (* m %) n))]
    (map f (range x0 x1 (/ (Math/abs (- x0 x1)) steps)))))


;; vector math

(defn v2-rotate
  "Rotate vector [x y] a radians counterclockwise."
  [a [x y]]
  (let [phi (atan2 y x)
        new-phi (+ phi a)
        r (sqrt (+ (* x x) (* y y)))]
    [(* r (cos new-phi))
     (* r (sin new-phi))]))

(defn v2*
  "Scalar multiplation with a vector"
  [s [x y]]
  [(* s x) (* s y)])

(defn v2+ [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


;; rand

(defn rrand
  "Return a random value between lower and upper."
  [lower upper]
  (+ lower (rand (- upper lower))))

(defn rand-nth
  "randomly pick an element from a sequence s (using nth)."
  [s]
  (nth s (rand-int (count s))))

(defn rand-circle-point
  "Return a random vector on the unit circle."
  []
  (let [a (rrand 0 (* 2 PI))]
    [(cos a) (sin a)]))


;; processing helpers

(let [img-size (atom [0 0])]
  (defn applet-resized?
    "return [width height] if size of *applet* had changed between this
  and the previous call to this function. Otherwise return nil"
    [] 
    (let [cur-size [(.width *applet*) (.height *applet*)]]
      (when (not= @img-size cur-size)
        (swap! img-size (constantly cur-size))))))

(let [img (atom nil)]
  (defn memoized-background
    "Return a fn, which should draw a static picture which will only
  be redrawn when a call to changed? returns logical true.
  Otherwise, a cached picture will be drawn with background-image.
  See also `applet-resized?' as a good candidate for changed? fn."
    ([f changed?]
       (if (changed?)
         (do (f)
             (swap! img (constantly (get-pixel))))
         (try (background-image @img) (catch Exception e nil))))))

(defn rgb [r g b]
  (.color *applet* (int r) (int g) (int b)))
