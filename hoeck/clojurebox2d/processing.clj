
;; processing related code for clojurebox

(ns hoeck.clojurebox2d.processing
  (:use rosado.processing
        (clojure.contrib pprint def))
  (:import (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter)
           (javax.swing JFrame JLabel JTextField JButton)
           (java.util.concurrent.atomic AtomicReference)
           (processing.core PApplet PFont)
           ;;(processing.opengl PGraphicsOpenGL)
           ))

;; sample draw function

(defn draw [] (fill 255)) ;; white

(defmacro with-applet 
  {:private true}
  [& body]
  `(binding [*applet* ~'this] ~@body))


;; font tools

(defn list-fonts
  "returns a list of available fonts, optionally filters them with
  regex (case-insensitive)."
  ([] (seq (PFont/list)))
  ([regex] (filter #(re-matches regex (.toLowerCase %))
                   (PFont/list))))

(defn pick-and-safe-font
  "Pick a font and safe it to disk, for further use with processing load-font.
  Return the name of the font or nil if no font was found.
  Pick the first if more than one font matches regex."
  [name-or-regex size smooth]
  (let [fnt-name (if (string? name-or-regex)
                   name-or-regex
                   (first (list-fonts name-or-regex)))
        papplet (PApplet.)
        pfont (if fnt-name (.createFont papplet fnt-name size smooth))]
    (when pfont
      (with-open [s (java.io.FileOutputStream. (str fnt-name ".font"))]
        (.save pfont s))
      fnt-name)))

(comment (pick-and-safe-font #".*free.*mono.*" 14 true))

;; event-hooks

(def key-pressed #())
(def key-released #())
(def key-typed #())

(def mouse-clicked #())
(def mouse-pressed #())
(def mouse-moved #())
(def mouse-released #())
(def mouse-dragged #())

;; keystatus

(def keystatus (atom #{})) ;; a set of keys currently pressed

;; build a map of keycodes -> keykeywords using reflection
(defn build-keycode-table []
  (let [key-ev-fields (.getFields java.awt.event.KeyEvent)
        codefields (filter #(.startsWith (.getName %) "VK") key-ev-fields)
        keycodes (map #(.get % nil) codefields)
        keynames (map #(-> % .getName (.substring 3) .toLowerCase keyword) codefields)]
    (zipmap keycodes keynames)))

(def keycodes (build-keycode-table))

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
               (size hsize vsize P3D)
               ;;(size hsize vsize OPENGL)
               )
               
             ;; anti-aliasing
             (if (:smooth opts) (smooth) (no-smooth))
             
             (no-stroke)
             (fill 0)
             (framerate (:framerate opts))
             
             (when-let [h (:setup-hook opts)] (h))

             ;; default font
             ;;(text-font (load-font "Free Monospaced.font"))
             ))
    (draw [] ;; default draw method
          (with-applet (draw)))
    
    ;; for each input-event, call the supermethod and only
    ;; implement the eventmethod with no args
    ;; -> to keep processing eventhandling working
    ;; for keypress & releases, update a keystatus set,
    ;; (to be able to read keys from the physics or draw loop)
    
    (keyPressed ([] (with-applet (key-pressed)))
                ([e] (swap! keystatus conj (keycodes (.getKeyCode e)))
                     (proxy-super keyPressed e)))

    (keyReleased ([] (with-applet (key-released)))
                 ([e] (swap! keystatus disj (keycodes (.getKeyCode e)))
                      (proxy-super keyReleased e)))

    (keyTyped ([] (with-applet (key-typed)))
              ([e] (proxy-super keyTyped e)))

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
  [:size [200 200] :smooth true :framerate 30 :applet-fn make-applet :setup-hook nil]
  (let [applet (applet-fn {:smooth smooth
                           :size size
                           :framerate framerate
                           :setup-hook setup-hook})
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

;; testing draw functions quickly in a dedicated processing environment

(def #^{:private :true} sketch-setup? (AtomicReference. nil))
(def #^{:private :true} current-sketch (AtomicReference. nil))

(defn- setup-sketch
  []
  (let [make-applet-fn (fn [opts]
                         (proxy [PApplet] []
                           (setup [] 
                                  (with-applet 
                                    (let [[hsize vsize] (:size opts)]
                                      (size hsize vsize P3D))
                                    (if (:smooth opts) (smooth) (no-smooth))
                                    (no-stroke)
                                    (fill 0)
                                    (framerate (:framerate opts))))
                           (draw [] (with-applet
                                      (let [f (.getAndSet current-sketch nil)]
                                        (when (ifn? f) (f)))))))
        [frm applet] (setup-processing :framerate 10 
                                       :size [320 200]
                                       :applet-fn make-applet-fn)]
    (.addWindowListener frm (proxy [WindowAdapter] []
                              (windowClosing [e] 
                                             (.set sketch-setup? nil)
                                             (.stop applet))))))

(defn show-sketch!
  "Execute f with *applet* bound to a PApplet in a separate frame.
  Useful for developing and testing graphics without 
  interrupting/redefining the main draw method."
  [f]
  (when (.compareAndSet sketch-setup? nil true) (setup-sketch))
  (.compareAndSet current-sketch nil f))

(defmacro with-buffer
  "Execute body within *applet* bound to a new PGraphics object.
  Wraps body in .beginDraw and .endDraw statements.
  Returns the newly created PGraphics.
  Useful for creating images without showing them in a frame during creation."
  [[size-x size-y] & body]
  `(let [g# (.createGraphics (PApplet.) ~size-x ~size-y P3D)]
     (binding [*applet* g#]
       (.beginDraw *applet*)
       ~@body
       (.endDraw *applet*))
     g#))

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





