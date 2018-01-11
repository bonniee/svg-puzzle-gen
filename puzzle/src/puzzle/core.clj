(ns puzzle.core
  (:require [puzzle.strings :as strings])
  (:require [puzzle.svg :as svg])
  (:require [puzzle.point :refer :all])
  (:require [voronoi-diagram.core :as voronoi])
  (:require [clojure.set :as set])
  (:require [kdtree]))

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

; Creates a string suitable for the "transform" argument of an SVG path element.
; Handles scaling, rotation, and translation.
(defn transform-string [origin angle line-length-ratio]
  (format strings/transform-template (x origin) (y origin) angle line-length-ratio))

; Draw a path from 0,0 to 100, 0, with squiggles based on cubic bezier curves in the middle
; Produces a string suitable for the "d" (aka description) attribute of an SVG path
; TODO: surely there's a better way to organize these sub-functions....
(defn squiggle-path-description
  []
  (let [
    squig-x-start 10.0
    squig-x-end 90.0
    cph (fn [] (+ (rand-int 10) 10.0)) ; Control point height, with random jitter
    neg-cph #(* -1 (cph)) ; Control point height, but negative
    s-phrase (fn [x h] (str " S " x " " h " " x " 0 ")) ; Convenience function for generating an S-term
    num-squigs (+ (rand-int 4) 5) ; Number of squiggles to draw
    dx (- squig-x-end squig-x-start)
    squig-width (/ dx num-squigs)

    first-move-line (str " M 0 0 " squig-x-start " 0 ")
    cp-jitter (fn [x] (+ (- (/ squig-width 4.0) (rand-int (/ squig-width 4.0)))) x)
    mk-first-control-path (fn [x-start x-end] (format " C %f %f %f %f %f 0 " (cp-jitter x-start) (neg-cph) (cp-jitter x-end) (cph) x-end))
    c-phrase-end (+ squig-width squig-x-start)
    first-control-path (mk-first-control-path squig-x-start c-phrase-end)

    s-starts (take-nth squig-width (range (+ squig-width c-phrase-end) squig-x-end))
    s-phrases (apply str (map-indexed (fn [i x] (s-phrase x (if (= 0 (mod i 2)) (cph) (neg-cph)))) s-starts))
    ]

    (str first-move-line first-control-path s-phrases (s-phrase squig-x-end (cph)) " M " squig-x-end " 0 100 0")))

; Creates an SVG element using a series of cubic bezier curves
(defn squiggle-path-svg [p1 p2]
  (let [
    line-length-ratio (/ (pointdist p1 p2) 100.0)
    transformed-string (transform-string p1 (angle p1 p2) line-length-ratio)
    path-d-attribute (squiggle-path-description)
    ; Scale by line-length in the x-direction,
    ; then rotate by $angle degrees around the (0, 0) point
    ]
  (format strings/path-template path-d-attribute transformed-string)
  ))

; Draws a puzzle-piece line for a given edge.
(defn puzzleline [edge]
  (squiggle-path-svg (nth edge 0) (nth edge 1)))

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

    ; TODO: draw puzzle lines extending from nearest neighbors to whimsy anchor points

    svgbody (concat puzzlelines WHIMSY_PATHS whimsy-anchors) ; this is what we're actually printing out

    ; Variables below are only used for debugging
    straight_line [[[500 500] [700 500]]]
    straight_line_points[[500 500] [700 500]]
    debug_line (map puzzleline straight_line)
    debug_points (map svg/point straight_line_points)
    debug_body (concat debug_line debug_points)]

    (svg/svg svgbody max-coord max-coord)
  ))
