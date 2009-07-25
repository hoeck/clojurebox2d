
;; processing related code for clojurebox

(ns hoeck.clojurebox2d.processing
  (:use rosado.processing
        (clojure.contrib pprint def))
  (:import (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter)
           (javax.swing JFrame JLabel JTextField JButton)
           (processing.core PApplet)))

;; sample draw function
(defn empty-draw []
  (fill 0))

(def draw empty-draw)

;; sample applet implementation
(defn make-applet []
  (proxy [PApplet] []
    (setup [] ;; default setup method
           (binding [*applet* this]
             ;(let [parent-size (.getSize (.getParent this))]
             ;  (size (.width parent-size) (.height parent-size) P3D))

             ;; initial size, P3D is the (fast) renderer
             (size 320 200 P3D)
             (smooth)
             (no-stroke)
             (fill 0)
             (framerate 30)))
    (draw [] ;; default draw method
          (binding [*applet* this]
            (draw)))))


(defnk setup-processing
  "Create a swing Frame, run a processing.core.PApplet applet
  inside and return the created frame."
  [applet :size [200 200] :smooth true]
  (let [[width height] size
        swing-frame (JFrame. "cljtest")]
    (.init applet)
    (doto swing-frame
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize width height)
      (.add applet)
      (.pack)
      (.setVisible true))
    swing-frame))


  ;(fill 226)
  ;(background-float 0 0 0)
  ;(fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  ;(ellipse 100 100 (rand-int 90) (rand-int 90))
  ;(stroke-float 10)
  ;(line 10 10 (+ 200 (rand-int 1000)) (+ 200 (rand-int 500)))
  ;(no-stroke)
  ;(filter-kind INVERT)



