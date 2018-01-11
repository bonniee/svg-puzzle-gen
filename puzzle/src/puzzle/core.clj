(ns puzzle.core
  (:require [puzzle.strings :as strings])
  (:require [puzzle.svg :as svg])
  (:require [puzzle.squiggle :as squiggle])
  (:require [puzzle.point :refer :all])
  (:require [voronoi-diagram.core :as voronoi])
  (:require [clojure.set :as set])
  (:require [kdtree]))

; TODO: solve duplicate point insertion bug

; Global variables for SVG dimensions
(def max-coord 1000)
(def N 10)

; Coords should be a set.
; Returns a set of points which conflict with the clipping circle around the provided point.
(defn disallowed_points [coords point radius]
  (set (filter (fn [candidate] (> radius (pointdist candidate point))) coords)))

; Removes all coordinates which include points conflicting with whimsy pieces.
; Whimsies should be a collection of Whimsy
(defn whimsy_disallowed_points [coords whimsies]
  (reduce concat
    (map (fn [w] (disallowed_points coords (.-origin w) (.-radius w))) whimsies)))

(defn points_from_edges [edges]
  (reduce concat
    (map (fn [edge] #{(nth edge 0) (nth edge 1)}) edges)))

; Whimsy: a whimsy puzzle piece
; origin: center point for placement
; radius: radius of the clipping circle for the whimsy
; svgpath: for drawing it
; anchors: points that should be connected to the rest of the puzzle
(defrecord Whimsy [origin radius svgpath anchors])
(def WHIMSIES (list 
  (Whimsy. [400 400] 250 strings/cat-whimsy (
    list
      [339.23 484.17] ; tail
      [384.24 386.15] ; back
      [436.84 321.39] ; left ear
      [465.03 459.87] ; forepaw
      [475.61 377.46] ; chest
          ))

  ; Circular whimsy
  ; (Whimsy. [700 800] 150 (point [700 800] :radius 50) (list [700 750] [700 850] [650 800] [750 800]))
  ))

(def WHIMSY_PATHS
  (apply str (map (fn [w] (.-svgpath w)) WHIMSIES)))

; Sets up coordinates for puzzle piece anchor points
(def base-coords
  (set (for [x (range N) y (range N)]
    (let [xcoord (+ 50 (* 100 x))
          ycoord (+ 50 (* 100 y))
          jitter 50
          xjitter (rand-int jitter)
          yjitter (rand-int jitter)
      ] (vec (list (+ xcoord xjitter) (+ ycoord yjitter)))))))

; Base coordinates, minus any pieces that conflict with whimsies
(def coords
  (vec
    (concat (map (fn [w] (.-origin w)) WHIMSIES) ; TODO: is there shorthand for this?
    (set/difference base-coords (whimsy_disallowed_points base-coords WHIMSIES)))))

; Draws a puzzle-piece line for a given edge.
(defn puzzleline [edge]
  (squiggle/squiggle-path-svg (nth edge 0) (nth edge 1)))

(defn whimsy_anchor_paths [points]
  (let [tree (kdtree/build-tree points)]
    (flatten (map (
      fn [whimsy] (map (fn [anchor]
        (let [neighbor (:point (nth (kdtree/nearest-neighbor tree anchor 2) 0))] ; todo double check this
          (puzzleline [anchor neighbor]))
        ; (point anchor :radius 3)
        ) (.-anchors whimsy))
      ) WHIMSIES))))

(defn -main
  []
  (let [
    {:keys [points edges cells]} (voronoi/diagram coords)
    puzzlelines (map puzzleline edges) ; draw puzzle lines based on voronoi edges
    simplelines (map (fn [p] (svg/line (nth p 0) (nth p 1) :color "blue")) edges) ; add this to see voronoi boundaries
    cell-lines (map svg/polygon-by-polygon-svg cells) ; add this to see voronoi cells (SHOULD be the same as simplelines)
    pointstrings (map svg/point coords) ; add this to see seed points
    whimsy-anchors (whimsy_anchor_paths (points_from_edges edges))
    svgbody (concat puzzlelines WHIMSY_PATHS whimsy-anchors) ; this is what we're actually printing out
    ]
    (svg/svg svgbody max-coord max-coord)
  ))
