(ns svgs)

(defn svg-prefix [width height]
  (let [prefix-string "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"%s\" height=\"%s\">"]
    (format prefix-string width height)
  )
)

(def svg-suffix "</svg>")

(defn ellipse [color]
  (let [ellipse-string "<ellipse
      cx=\"250.0\"
      cy=\"250.0\"
      rx=\"100.0\"
      ry=\"100.0\"
      stroke=\"#fc8d62\"
      stroke-width=\"5\"
      fill=\"#%s\"
    />"]
    (format ellipse-string color))
)

(defn rand-color []
  (let [colors ["66c2a5" "fc8d62" "8da0cb"]]
    (nth colors (rand-int (count colors)))
    ))

(defn svg [body width height]
  (println (svg-prefix width height))
  (println body)
  (println svg-suffix))

(println (svg (ellipse (rand-color)) 200 200))