(ns wonderland.core
  (:require [voronoi-diagram.core :as voronoi])
  (:require [clojure.set :as set]))

; Global variables for SVG dimensions
(def max-coord 1000)
(def N 10)

; Convenience extractors for points of the form [x y]
(defn x [point] (double (nth point 0)))
(defn y [point] (double (nth point 1)))

(defn pointdist [p1 p2]
  (let [
    x1 (x p1)
    x2 (x p2)
    y1 (y p1)
    y2 (y p2)
    dx (- x2 x1)
    dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

; Draw an SVG ellipse element representing a point
; Expects a coord of form [x y]
(defn point [coord & {:keys [radius] :or {radius 1}}]
  (let [
    ellipse-string "<ellipse
      cx=\"%.2f\"
      cy=\"%.2f\"
      rx=\"%d\"
      ry=\"%d\"
      stroke=\"#fc8d62\"
      stroke-width=\"1\"
      fill=\"transparent\"
    />"]
    (format ellipse-string (x coord) (y coord) radius radius))
)

; Coords should be a set.
; Returns a set of points which conflict with the clipping circle around the provided point.
(defn disallowed_points [coords point radius]
  (set (filter (fn [candidate] (> radius (pointdist candidate point))) coords)))

; Removes all coordinates which include points conflicting with whimsy pieces.
; Whimsies should be a collection of Whimsy
(defn whimsy_disallowed_points [coords whimsies]
  (reduce concat
    (map (fn [w] (disallowed_points coords (.-origin w) (.-radius w))) whimsies)))

; Whimsy: a whimsy puzzle piece
; origin: center point for placement
; radius: radius of the clipping circle for the whimsy
; svgpath: for drawing it
; anchors: points that should be connected to the rest of the puzzle
(defrecord Whimsy [origin radius svgpath anchors])
(def WHIMSIES (list 
  (Whimsy. [400 400] 250 (point [400 400] :radius 100) (list [500 400] [300 400] [400 300] [400 500]))
  (Whimsy. [700 800] 150 (point [700 800] :radius 50) (list [700 750] [700 850] [650 800] [750 800]))
  ))

(def WHIMSY_PATHS
  (apply str (map (fn [w] (.-svgpath w)) WHIMSIES)))

(def WHIMSY_ANCHOR_PATHS
  (flatten (map (
    fn [whimsy] (map (fn [anchor] (point anchor :radius 3)) (.-anchors whimsy))
    ) WHIMSIES))
  )

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

; Prefix for SVG file
(defn svg-prefix [width height]
  (let [prefix-string "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"%s\" height=\"%s\">"]
    (format prefix-string width height)
  )
)

; Suffix for SVG file
(def svg-suffix "</svg>")

; Create an SVG, suitable for file output.
(defn svg [body]
  (println (svg-prefix max-coord max-coord))
  (println (apply str body))
  (println svg-suffix))



; Draw an SVG element representing a line from p1 to p2
; Expects each point to be of the form [x y]
(defn line [p1 p2 & {:keys [color] :or {color "black"}}]
  (let [
    line-string "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\"
      stroke-width=\"2\" stroke=\"%s\"/>"
      x1 (x p1)
      y1 (y p1)
      x2 (x p2)
      y2 (y p2)
      ]
      (format line-string x1 y1 x2 y2 color)
      )
  )

; DONT USE ME
; This is a simple renderer for a polygon SVG element.
; Mostly useless for puzzle-building purposes but kept here for debugging.
(defn polygon-by-polygon-svg [points]
  (let [
    polygon-string "<polygon fill=\"none\" stroke=\"black\" stroke-width=\"2\" points=\"%s\"/>"
    points-string (apply str (map (fn [p] (format "%.2f,%.2f " (x p) (y p))) points))
    ]
    (format polygon-string points-string))
  )

; Creates a string suitable for the "transform" argument of an SVG path element.
; Handles scaling, rotation, and translation.
(defn transform-string
  [x y angle line-length-ratio]
  (format "translate(%f %f) rotate (%f 0 0) scale (%f 1)" x y angle line-length-ratio))

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
    x1 (x p1)
    y1 (y p1)
    x2 (x p2)
    y2 (y p2)
    dx (- x2 x1)
    dy (- y2 y1)
    line-length (Math/sqrt (+ (* dx dx) (* dy dy)))
    line-length-ratio (/ line-length 100.0)
    angle (* (/ 180 Math/PI) (Math/atan2 dy dx))
    midx (+ (min x1 x2) dx)
    yjitter 20
    midheight (rand-int yjitter)
    transformed-string (transform-string x1 y1 angle line-length-ratio)
    cph 20
    path-template (squiggle-path-description)
    ; Scale by line-length in the x-direction,
    ; then rotate by $angle degrees around the (0, 0) point
    path-string "<path
                  d=\"%s\" stroke=\"black\" fill=\"transparent\"
                  transform=\"%s\"/>
                  "
    ]
  (format path-string path-template transformed-string)
  ))

; Draws a puzzle-piece line for a given edge.
(defn puzzleline [edge]
  (squiggle-path-svg (nth edge 0) (nth edge 1)))

(defn -main
  []
  (let [
    {:keys [points edges cells]} (voronoi/diagram coords)
    puzzlelines (map puzzleline edges) ; draw puzzle lines based on voronoi edges
    simplelines (map (fn [p] (line (nth p 0) (nth p 1) :color "blue")) edges) ; add this to see voronoi boundaries
    cell-lines (map polygon-by-polygon-svg cells) ; add this to see voronoi cells (SHOULD be the same as simplelines)
    pointstrings (map point coords) ; add this to see seed points

    ; TODO: draw puzzle lines extending from nearest neighbors to whimsy anchor points

    svgbody (concat puzzlelines WHIMSY_PATHS WHIMSY_ANCHOR_PATHS) ; this is what we're actually printing out

    ; Variables below are only used for debugging
    straight_line [[[500 500] [700 500]]]
    straight_line_points[[500 500] [700 500]]
    debug_line (map puzzleline straight_line)
    debug_points (map point straight_line_points)
    debug_body (concat debug_line debug_points)]

    (svg svgbody)
  ))
