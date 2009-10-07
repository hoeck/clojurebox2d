
(ns hoeck.clojurebox2d.processing.colors
  (:use hoeck.clojurebox2d.processing
        rosado.processing))

(def color {})

(defn- pcolor [x y z]
  (.color *applet* x y z))

(defn color-setup []
  (color-mode HSB 100 100 100 100)
  (let [c {:red (pcolor 0 100 100)           
           :green (pcolor 28 100 100)
           :blue (pcolor 58 100 100)
           :yellow (pcolor 17 100 100)

           :black (pcolor 0 0 0)
           :grey (pcolor 0 40 40)
           :white (pcolor 0 0 100)}]
    (alter-var-root #'color (constantly c))))

(comment
  (show-sketch!
   #(do (color-mode HSB 100 100 100 100)
        (rect-mode CENTER)
        (background-int (.color *applet* 58 50 50) 0)
        (let [a (range 0 100 1)]
          (dorun (map (fn [hue [x y]]
                        (fill-float hue 100 100)
                        (rect x y 10 10))
                      a
                      (grid-points [0 0] [1000 30] [100 1]))))
        (println 'ok))))


