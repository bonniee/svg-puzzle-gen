(ns example.core
  (:require [voronoi-diagram.core :as voronoi]))

(let [points [[2 2] [1 4] [4 1] [-10 -10] [-10 10] [10 10] [10 -10]]
      {:keys [points edges cells]} (voronoi/diagram points)]
  (println "points" (nth points 1))
  (println "edge" (nth edges 1))
  (println "cell" (nth cells 1) ))


(let [points [[0 0] [1 1] [0 1]]
      {:keys [points edges cells]} (voronoi/diagram points)]
  (println "points" points 1)
  (println "edge" edges)
  (println "cell" cells 1))