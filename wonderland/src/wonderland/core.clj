(ns wonderland.core
  (:require [voronoi-diagram.core :as voronoi]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main
  []
  (foo "from main:")
  (let [points [[0 0] [1 1] [0 1]]
      {:keys [points edges cells]} (voronoi/diagram points)]
  (println "points" points 1)
  (println "edge" edges)
  (println "cell" cells 1))
  )