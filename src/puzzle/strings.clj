(ns puzzle.strings
  "puzzle.strings contains string templates, mostly consisting of partial SVG paths."
  )

(def cat-whimsy "
  <path id=\"cat\" fill=\"none\" stroke=\"#000000\" stroke-width=\"1\" stroke-miterlimit=\"10\" d=\"M55.571,141.777l55.54,0.994
        c0.142-7.894-0.396-9.472-13.47-9.706c2.201-6.229,10.96-21.235,15.679-21.15c4.177,0.075,9.152,0.395,8.982,9.915
        c-0.193,10.812,17.689,28.577,22.23,21.538c6.766-10.479-11.173-7.564-10.715-33.132c0.626-34.97,20.944-29.626,21.295-49.276
        c0.176-9.83-2.514-11.206-2.388-18.234c0.167-9.286,8.34-8.601,6.885-16.331c-1.006-5.339-1.79-9.62-2.313-16.064
        c-0.375-4.573-0.436-9.441-5.177-9.33c-5.573,0.13-7.862,9.897-16.535,10.445c-8.652,0.547-15.876-7.642-19.244-6.551
        c-3.333,1.082-2.453,10.097-0.601,16.521c2.912,10.098,9.417,23.274-2.906,25.02c-12.323,1.746-33.492,3.334-49.089,23.213
        c-15.597,19.878-15.46,43.54-21.557,50.747c-20.472,24.196-40.82,13.749-41.186,34.17c-0.164,9.146,15.213,16.005,17.724,13.1
        c2.51-2.905-20.671-12.105,5.628-22.024C46.823,137.176,48.846,135.403,55.571,141.777z\"
        transform=\"translate(320.5 316.5)\"
        />")

(def ellipse-template "<ellipse
      cx=\"%.2f\"
      cy=\"%.2f\"
      rx=\"%d\"
      ry=\"%d\"
      stroke=\"#fc8d62\"
      stroke-width=\"1\"
    />")

(def svg-prefix-template "<svg xmlns=\"http://www.w3.org/2000/svg\"
  xmlns:xlink=\"http://www.w3.org/1999/xlink\"
  width=\"%s\" height=\"%s\">")

(def svg-suffix "</svg>")

(def line-template "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\"
      stroke-width=\"2\" stroke=\"%s\"/>")

(def polygon-template "<polygon fill=\"none\" stroke=\"black\" stroke-width=\"2\" points=\"%s\"/>")

(def transform-template "translate(%f %f) rotate (%f 0 0) scale (%f 1)")

(def path-template "<path
  d=\"%s\" stroke=\"black\" fill=\"transparent\"
  transform=\"%s\"/>
  ")

