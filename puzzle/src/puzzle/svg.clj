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
