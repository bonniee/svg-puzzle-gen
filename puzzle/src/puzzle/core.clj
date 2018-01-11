(ns puzzle.core
  (:require [voronoi-diagram.core :as voronoi])
  (:require [clojure.set :as set])
  (:require [kdtree]))

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

(def CATWHIMSY "
  <path id=\"cat\" fill=\"none\" stroke=\"#000000\" stroke-width=\"1\" stroke-miterlimit=\"10\" d=\"M55.571,141.777l55.54,0.994
        c0.142-7.894-0.396-9.472-13.47-9.706c2.201-6.229,10.96-21.235,15.679-21.15c4.177,0.075,9.152,0.395,8.982,9.915
        c-0.193,10.812,17.689,28.577,22.23,21.538c6.766-10.479-11.173-7.564-10.715-33.132c0.626-34.97,20.944-29.626,21.295-49.276
        c0.176-9.83-2.514-11.206-2.388-18.234c0.167-9.286,8.34-8.601,6.885-16.331c-1.006-5.339-1.79-9.62-2.313-16.064
        c-0.375-4.573-0.436-9.441-5.177-9.33c-5.573,0.13-7.862,9.897-16.535,10.445c-8.652,0.547-15.876-7.642-19.244-6.551
        c-3.333,1.082-2.453,10.097-0.601,16.521c2.912,10.098,9.417,23.274-2.906,25.02c-12.323,1.746-33.492,3.334-49.089,23.213
        c-15.597,19.878-15.46,43.54-21.557,50.747c-20.472,24.196-40.82,13.749-41.186,34.17c-0.164,9.146,15.213,16.005,17.724,13.1
        c2.51-2.905-20.671-12.105,5.628-22.024C46.823,137.176,48.846,135.403,55.571,141.777z\"
        transform=\"translate(320.5 316.5)\"
        />
  ")

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
  (Whimsy. [400 400] 250 CATWHIMSY (
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
    simplelines (map (fn [p] (line (nth p 0) (nth p 1) :color "blue")) edges) ; add this to see voronoi boundaries
    cell-lines (map polygon-by-polygon-svg cells) ; add this to see voronoi cells (SHOULD be the same as simplelines)
    pointstrings (map point coords) ; add this to see seed points
    whimsy-anchors (whimsy_anchor_paths (points_from_edges edges))

    ; TODO: draw puzzle lines extending from nearest neighbors to whimsy anchor points

    svgbody (concat puzzlelines WHIMSY_PATHS whimsy-anchors) ; this is what we're actually printing out

    ; Variables below are only used for debugging
    straight_line [[[500 500] [700 500]]]
    straight_line_points[[500 500] [700 500]]
    debug_line (map puzzleline straight_line)
    debug_points (map point straight_line_points)
    debug_body (concat debug_line debug_points)]

    (svg svgbody)
  ))
