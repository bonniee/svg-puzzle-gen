;;; clojure -i points.clj > points.svg

(ns svgs)

(defn svg-prefix [width height]
  (let [prefix-string "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"%s\" height=\"%s\">"]
    (format prefix-string width height)
  )
)

(def svg-suffix "</svg>")

(defn ellipse [color width height]
  (let [
    cx (/ width 2.0)
    cy (/ height 2.0)
    rx (* 0.7 (/ width 2.0))
    ry (* 0.7 (/ height 2.0))
    ellipse-string "<ellipse
      cx=\"%.1f\"
      cy=\"%.1f\"
      rx=\"%.1f\"
      ry=\"%.1f\"
      stroke=\"#fc8d62\"
      stroke-width=\"5\"
      fill=\"#%s\"
    />"]
    (format ellipse-string cx cy rx ry color))
)

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


(defn rand-color []
  (let [colors ["66c2a5" "fc8d62" "8da0cb"]]
    (nth colors (rand-int (count colors)))
    ))

(defn svg [body width height]
  (println (svg-prefix width height))
  (println body)
  (println svg-suffix))

(def max-coord 1000)
(def grid 100)
(def point-count 10)

(svg
 (apply str 
(for [x (range 10) y (range 10)] 
  (let [xcoord (+ 50 (* 100 x))
        ycoord (+ 50 (* 100 y))
    ] (point xcoord ycoord)))

) max-coord max-coord)

; (svg (repeatedly point-count #(point (rand-color) (rand-int max-coord) (rand-int max-coord))) max-coord max-coord)
