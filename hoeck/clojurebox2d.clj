
(ns hoeck.clojurebox2d
  (:use hoeck.clojurebox2d.processing
        hoeck.thread
        hoeck.iterate
        rosado.processing
        (clojure.contrib pprint except def))
  (:require [hoeck.clojurebox2d.jbox2d :as jbox])
  (:import (processing.core PApplet)

           (java.awt.event MouseWheelEvent MouseWheelListener WindowAdapter
                           ComponentListener)
           (javax.swing JFrame JLabel JTextField JButton)
           
           (java.util.concurrent ArrayBlockingQueue ConcurrentLinkedQueue)
           
           (org.jbox2d.collision.shapes Shape PolygonShape CircleShape)
           (org.jbox2d.dynamics Body World)))

;; clojurebox environment

(def cbox {:frame nil;; the JFrame containing the PApplet
           :applet nil;; PApplet processing environment
           })

(defmacro with-applet
  "Evaluate body with *applet* bound to the current PApplet
  (the processing environment, (:applet cbox)"
  [& body]
  `(binding [*applet* (:applet cbox)]
     ~@body))

(declare window-closing window-resized)
(defn init
  "initialize clojurebox2d. Start processing and create dedicated jbox2d thread.
  Call f within this-thread. Return the created Thread."
  ([] (init {}))
  ([opts-map]
     (let [;; creates its own render thread, returns the frame and applet           
           [frm app] (setup-processing :size (:size opts-map [320 200])
                                       :framerate (:framerate opts-map 30)
                                       :smooth (:smooth opts-map true)
                                       :setup-hook (:setup-hook opts-map))
           ;; creates but doesn't start the world-thread
           world-thread (jbox/make-jbox2d-thread opts-map)]
       ;; set window listeners
       (.addWindowListener frm (proxy [WindowAdapter] []
                                 (windowClosing [e] (with-applet (window-closing)))))
       (.addComponentListener frm (proxy [ComponentListener] []
                                    (componentResized [e] (with-applet (window-resized)))
                                    (componentMoved [e])
                                    (componentHidden [e])
                                    (componentShown [e])))
       (.start world-thread)
       (alter-var-root #'cbox merge
                       {:jbox-thread world-thread
                        :frame frm
                        :applet app}))))

;; set hooks into the various places of clojurebox
(def cljbox-hook-vars {'window-resized `window-resized,
                       'window-closing `window-closing
                       'jbox2d-loop `jbox/jbox2d-loop})

(defn redef*
  {:doc (str "Use f as the new definition of the function name." \newline
             "  Redefinable functions include:" \newline
             (apply str (map #(str "    " % \newline) (keys processing-methods))) \newline
             "  for processing and " \newline
             (apply str (map #(str "    " % \newline) (keys cljbox-hook-vars))) \newline
             "  for clojurebox2d hooks. See the docstring of those functions for" \newline
             "  more information.")}
  [hook-name f]
  (let [full-name (resolve (or (processing-methods hook-name)
                               (cljbox-hook-vars hook-name)
                               (throwf "unknown hook: %s" hook-name)))]
    (if full-name 
      (alter-var-root full-name (constantly f))
      (throwf "Unknown place: %s" full-name))))

(defmacro redef
  "Redefine a (usually) argless hook-function. See (doc redef*) for
  possible functions. Processing functions are usually executed within the
  processing thread and a bound *applet*."
  [hook-name argvec & body]
  `(redef* '~hook-name (fn ~argvec ~@body)))

;; default window events

(defn window-resized
  "Called when the clojurebox frame is resized. Defaults to do nothing."
  [])

(defn window-closing
  "Called when the clojurebox frame is about to be closed. By default, 
  interrupt the jbox2d thread and call .stop on the processing applet."
  []
  ;; when closing the frame
  ;; stop the physhics sim and processing
  (println :window-closing)
  (.interrupt (:jbox-thread cbox))
  (with-applet (.stop *applet*)))

(println "clojurebox2d")



