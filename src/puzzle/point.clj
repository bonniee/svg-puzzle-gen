(ns puzzle.point
  "Convenience functions for working with coordinates"
  )

(defn x 
  "Convenience extractor for the x value of a coordinate [x y]"
  [point] (double (nth point 0)))

(defn y
  "Convenience extractor for the y value of a coordinate [x y]"
  [point] (double (nth point 1)))

(defn pointdist
  "Calculate the distance between two coordinates"
  [p1 p2]
  (let [
    x1 (x p1)
    x2 (x p2)
    y1 (y p1)
    y2 (y p2)
    dx (- x2 x1)
    dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn angle
  "Calculate the angle of a line segment between two given points p1 and p2"
  [p1 p2]
  (let [
    x1 (x p1)
    x2 (x p2)
    y1 (y p1)
    y2 (y p2)
    dx (- x2 x1)
    dy (- y2 y1)]
    (* (/ 180 Math/PI) (Math/atan2 dy dx))))
