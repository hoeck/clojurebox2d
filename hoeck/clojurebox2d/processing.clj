
;; processing related code for clojurebox

(ns hoeck.clojurebox2d.processing
  (:use rosado.processing
        (clojure.contrib pprint def))
  (:import (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter)
           (javax.swing JFrame JLabel JTextField JButton)
           (processing.core PApplet)))

;; sample draw function

(defn draw [] (fill 255)) ;; white

(defmacro with-applet 
  {:private true}
  [& body]
  `(binding [*applet* ~'this] ~@body))

;; event-hooks

(def key-pressed #())
(def key-released #())
(def key-typed #())

(def mouse-clicked #())
(def mouse-pressed #())
(def mouse-moved #())
(def mouse-released #())
(def mouse-dragged #())

;; sample applet implementation
(defn- make-applet [opts]
  (proxy [PApplet] [] ;; processing.core.PApplet
    (setup [] ;; default setup method
           (with-applet
            ;; doesn't work:
            ;;(let [parent-size (.getSize (.getParent this))]
            ;;  (size (.width parent-size) (.height parent-size) P3D))

            ;; initial size, P3D is the (fast) renderer
            (let [[hsize vsize] (:size opts)]
              (size hsize vsize P3D))

            ;; anti-aliasing
            (if (:smooth opts) (smooth))
            (no-stroke)
            (fill 0)
            (framerate (:framerate opts))))
    (draw [] ;; default draw method
          (with-applet (draw)))
    
    ;; for each input-event, call the supermethod and only
    ;; implement the eventmethod with no args
    ;; -> to keep processing eventhandling working
    
    (keyPressed ([] (with-applet (key-pressed)))
                ([e] (proxy-super keyPressed)))

    (keyReleased ([] (with-applet (key-released)))
                 ([e] (proxy-super keyReleased)))

    (keyTyped ([] (with-applet (key-typed)))
              ([e] (proxy-super keyTyped)))

    (mousePressed ([] (with-applet (mouse-pressed)))
                  ([e] (proxy-super mousePressed e)))

    (mouseReleased ([] (with-applet (mouse-released)))
                  ([e] (proxy-super mouseReleased e)))

    (mouseClicked ([] (with-applet (mouse-clicked)))
                  ([e] (proxy-super mouseClicked e)))
    
    (mouseDragged ([] (with-applet (mouse-dragged)))
                  ([e] (proxy-super mouseDragged e)))
    
    (mouseMoved ([] (with-applet (mouse-moved)))
                ([e] (proxy-super mouseMoved e)))))

(defnk setup-processing
  "Create a swing Frame, create a processing.core.PApplet applet
  inside and return the created objects [applet, frame]."
  [:size [200 200] :smooth true :framerate 30]
  (let [applet (make-applet {:smooth smooth
                             :size size
                             :framerate framerate})
        [width height] size
        swing-frame (JFrame. "cljtest")]
    (.init applet)
    (doto swing-frame
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize width height)
      (.add applet)
      (.pack)
      (.setVisible true))
    [swing-frame applet]))

  ;(fill 226)
  ;(background-float 0 0 0)
  ;(fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  ;(ellipse 100 100 (rand-int 90) (rand-int 90))
  ;(stroke-float 10)
  ;(line 10 10 (+ 200 (rand-int 1000)) (+ 200 (rand-int 500)))
  ;(no-stroke)
  ;(filter-kind INVERT)

;; methods to overwrite in PApplet
(def processing-methods
     {'setup `setup
      'draw `draw
      
      'key-pressed `key-pressed
      'key-released `key-released
      'key-typed `key-typed
      
      'mouse-clicked `mouse-clicked
      'mouse-dragged `mouse-dragged
      'mouse-moved `mouse-moved
      'mouse-pressed `mouse-pressed
      'mouse-released `mouse-released})





