
(ns hoeck.clojurebox2d.processing.widgets
  (:use clojure.contrib.def
        rosado.processing
        hoeck.iterate
        hoeck.clojurebox2d.processing.utils))

;; widget layout

(defn place-widget
  "place a widget on the processing screen.
  horizontal alignment: :left or :right
  vertical alignment: :top or :bottom."
  ;; eg: (place-widget w :left :top)
  [widget horiz-align vert-align border-px]
  (let [[sx sy] (v2* 0.5 (widget))
        x (if (= horiz-align :left)
            (+ border-px sx)
            (- (.width *applet*) border-px sx))
        y (if (= vert-align :top)
            (+ border-px sy)
            (- (.height *applet*) border-px sy))]    
    (widget [x y])))

;; widgets

(defnk make-levelmeter
  "Return a rectangle shaped bar widget to display a value between 0 and 1."
  [:a-data-fn nil ;; fn returning a float value between 0 and 1,
   :color-a 0 ;; color for A area
   :color-b 0 ;; color for non A area
   :stroke-color 0 ;; border color
   :direction :vertical
   :size [20 7]]
  (let [[x0 y0] (v2* -0.5 size)
        [xs ys] size]
    (fn levelmeter
      ([] size)
      ([[x y]]
         (let [val (or (a-data-fn) 0)]
           (with-translation [x y]
             (stroke-int stroke-color)
             (fill-int color-b )
             (rect-mode CORNER)
             (rect x0 y0 xs ys)
             (stroke-int color-a)
             (fill-int color-a)
             (when (<= 0 val)
               (if (= direction :vertical)
                 (rect (inc x0) (inc y0) (- xs 2) (* val (- ys 2)))
                 (rect (inc x0) (inc y0) (* val (- xs 2)) (- ys 2))))))))))

(defn collect
  "given a function f, an initial value val and a sequence coll, compute a
  resulting sequence sn: s0 = (f val coll0) and sn = (f sn-1 colln)."
  ([f val coll]
     (lazy-seq 
       (when (seq coll)
         (let [v (f val (first coll))]
           (cons v (collect f v (rest coll))))))))

(defn stack-widgets
  "Return a widget wich stacks given widgets.
  Possible directions: :horizontal or :vertical
  Possible alignments: :top/:bottom for :horizontal and
                       :left/:right for :vertical"
  [direction alignment & widgets]
  (let [sizes (map #(%) widgets)
        gap-px 6
        vertical? (= direction :vertical) ;; opposite: :horizontal
        align-top-left? (#{:left :bottom} alignment)
        stacksize (if vertical?
                    [(apply max (map first sizes))
                     (- (reduce (partial + 5) 0 (map second sizes)) gap-px)]
                    [(- (reduce (partial + 5) 0 (map first sizes)) gap-px)
                     (apply max (map second sizes))])
        [max-stack-x max-stack-y] stacksize
        compute-direction-points (fn [sizes-in-direction]
                                   (map (partial + gap-px) 
                                        (map first 
                                             (partition 2 
                                                        (collect + 0 
                                                                 (mapcat #(let [half (-> % (+ gap-px) (* 0.5))]
                                                                            [half half])
                                                                         sizes-in-direction))))))
        x-positions (if vertical?
                      (map (if align-top-left?
                             #(* 0.5 (- max-stack-x %))
                             #(* -0.5 (- max-stack-x %)))
                           (map first sizes))
                      (map #(- (* 0.5 max-stack-x) %)
                           (compute-direction-points (map first sizes))))
        y-positions (if vertical?
                      (map #(- (* 0.5 max-stack-y) %)
                           (compute-direction-points (map second sizes)))
                      (map (if align-top-left?
                             #(* 0.5 (- max-stack-y %))
                             #(* -0.5 (- max-stack-y %)))
                           (map second sizes)))
        positions (map vector x-positions y-positions)]
    (fn widget-stack
      ([] stacksize)
      ([v]
         (with-translation v
           (iter (for p in positions)
                 (for w in widgets)
                 (do (w p)
                     (recur))))))))

