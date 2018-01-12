(ns puzzle.core
  (:require [puzzle.strings :as strings])
  (:require [puzzle.svg :as svg])
  (:require [puzzle.squiggle :as squiggle])
  (:require [puzzle.circular-coords :as circular-coords])
  (:require [puzzle.grid-coords :as grid-coords])
  (:require [puzzle.point :refer :all])
  (:require [puzzle.whimsy :as whimsy])
  (:require [voronoi-diagram.core :as voronoi])
  (:require [clojure.set :as set])
  (:require [kdtree]))

; Global variables for SVG dimensions
(def max-coord 1000)
(def N (/ max-coord 100))

; For use with voronoi edges
(defn points_from_edges [edges]
  (reduce concat
    (map (fn [edge] #{(nth edge 0) (nth edge 1)}) edges))) ; TODO: duplicate set insertion bug here

; Sets up coordinates for puzzle piece anchor points
(def base-coords (grid-coords/seed-coords 10 :jitter 30))
; (def base-coords (circular-coords/seed-coords 6))

(defn puzzle
  "Generates a puzzle"
  [base-coords whimsies]
  (let [
    coords (whimsy/whimsy_coords base-coords whimsies)
    {:keys [points edges cells]} (voronoi/diagram coords)
    
    svgbody (concat
      (map squiggle/squiggle-path-svg edges)
      (whimsy/whimsy-paths whimsies)
      (whimsy/whimsy_anchor_paths (points_from_edges edges) whimsies))]

    (svg/svg svgbody max-coord max-coord)
  ))

(defn -main [] (puzzle base-coords whimsy/WHIMSIES))

