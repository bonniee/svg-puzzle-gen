(ns wonderland.core
  (:require [voronoi-diagram.core :as voronoi]))

(def max-coord 500)

; Convenience extractors for points of the form [x y]
(defn x [point] (nth point 0))
(defn y [point] (nth point 1))

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

; Draw the body of a point for SVG output
(defn point [x y]
  (let [
    radius 3
    ellipse-string "<ellipse
      cx=\"%d\"
      cy=\"%d\"
      rx=\"%d\"
      ry=\"%d\"
      stroke=\"#fc8d62\"
      stroke-width=\"5\"
    />"]
    (format ellipse-string x y radius radius))
)

(defn line [p1 p2]
  (let [
    line-string "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\"
      stroke-width=\"2\" stroke=\"black\"/>"
      x1 (x p1)
      y1 (y p1)
      x2 (x p2)
      y2 (x p2)
      ]
      (format line-string x1 y1 x2 y2)
      )
  )

; Create an SVG, suitable for file output.
(defn svg [body]
  (println (svg-prefix max-coord max-coord))
  (println (apply str body))
  (println svg-suffix))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main
  []
  (svg (line [0 0] [100 100]))
)
