(ns wonderland.core
  (:require [voronoi-diagram.core :as voronoi]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main
  []
  (foo "from main:")
  
  )