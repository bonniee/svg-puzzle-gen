(ns puzzle.svg
  (:require [puzzle.strings :as strings])
  (:require [puzzle.point :refer :all]))

; Prefix for SVG file
(defn- svg-prefix [width height]
  (format strings/svg-prefix-template width height))

; Create an SVG, suitable for file output.
(defn svg [body width height]
  (println (svg-prefix width height))
  (println (apply str body))
  (println strings/svg-suffix))

; Draw an SVG element representing a line from p1 to p2
; Expects each point to be of the form [x y]
(defn line [p1 p2 & {:keys [color] :or {color "black"}}]
  (let [
      x1 (x p1)
      y1 (y p1)
      x2 (x p2)
      y2 (y p2)
      ]
      (format strings/line-template x1 y1 x2 y2 color)
      )
  )

; Draw an SVG ellipse element representing a point
; Expects a coord of form [x y]
(defn point [coord & {:keys [radius] :or {radius 1}}]
  (format strings/ellipse-template (x coord) (y coord) radius radius)
)

; DONT USE ME
; This is a simple renderer for a polygon SVG element.
; Mostly useless for puzzle-building purposes but kept here for debugging.
(defn polygon-by-polygon-svg [points]
  (let [
    points-string (apply str (map (fn [p] (format "%.2f,%.2f " (x p) (y p))) points))
    ]
    (format strings/polygon-template points-string))
  )

; Creates a string suitable for the "transform" argument of an SVG path element.
; Handles scaling, rotation, and translation.
(defn- transform-string [origin angle line-length-ratio]
  (format strings/transform-template (x origin) (y origin) angle line-length-ratio))

; Draws a path between two points,
; provided the start and end points and a description
; of the path.
; base-length represents the length of the described path;
; the described path will be scaled and rotated according to the
; start and end points.
(defn svg-path [start end description base-length]
  (let [length-ratio (/ (pointdist start end) base-length)
        transformed-string (transform-string
          start (angle start end) length-ratio)]
        (format strings/path-template description transformed-string)))

(defn s-path-phrase [x1 y1 x2 y2]
  (str " S " x1 " " y1 " " x2 " " y2))

(defn c-path-phrase [x1 y1 x2 y2 x3 y3]
  (str " C " x1 " " y1 " " x2 " " y2 " " x3 " " y3 " "))

(defn m-path-phrase [x1 y1 x2 y2]
  (str " M " x1 " " y1 " " x2 " " y2))


(ns puzzle.squiggle
  (:require [puzzle.svg :refer :all]))

; This file is for drawing a squiggle.

(def x-start 10.0)

(def x-end 90.0)

(def dx (- x-end x-start))

(defn- control-point-height []
  (+ (rand-int 10) 10.0))

(defn- neg-control-point-height []
              (* -1 (control-point-height)))

(defn- squiggle-s-phrase [x h]
              (s-path-phrase x h x 0))

; Adds +/- jitter to base value
(defn- jitter [base jitter]
  (+ base (- jitter (rand-int (* 2 jitter)))))

; TODO play around with this
(defn- control-point-x-jitter [x squig-width]
  (jitter x (/ squig-width 2.0)))

(defn- first-c-phrase [squig-width]
  (let [x-end (+ squig-width x-start)]
    (c-path-phrase
          (control-point-x-jitter x-start squig-width)
          (neg-control-point-height)
          (control-point-x-jitter x-end squig-width)
          (control-point-height)
          x-end
          0)))

(defn- s-partitions [squig-width]
  (let [s-phrase-start (+ squig-width (+ squig-width x-start))]
    (take-nth squig-width (range s-phrase-start x-end))))

(defn- s-phrases [squig-width]
  (apply str
    (map-indexed (fn [i x]
      (s-path-phrase
        x
        (if (= 0 (mod i 2)) (control-point-height) (neg-control-point-height))
        x
        0
        ))
      (s-partitions squig-width))))

; Create the d attribute for a squiggle path
(defn- squiggle-path-d []
  (let [num-squigs (+ (rand-int 4) 5)
        squig-width (/ dx num-squigs)]
        (str  (m-path-phrase 0 0 x-start 0)
              (first-c-phrase squig-width)
              (s-phrases squig-width)
              (s-path-phrase x-end (control-point-height) x-end 0)
              (m-path-phrase x-end 0 100 0))))

; Creates an SVG element using a series of cubic bezier curves
(defn squiggle-path-svg [p1 p2]
  (svg-path p1 p2 (squiggle-path-d) 100.0))