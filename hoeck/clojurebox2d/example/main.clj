

(ns hoeck.clojurebox2d.example.main
  (:use hoeck.clojurebox2d.example
        hoeck.clojurebox2d)
  (:gen-class :main true))

(defn -main [& args-ignored]
  (println "Clojure Space Duel")
  (println "Player 1 (red): arrow keys and space to fire")
  (println "Player 2 (blue): WASD keys and Q to fire")
  (clojure-space-duel))


