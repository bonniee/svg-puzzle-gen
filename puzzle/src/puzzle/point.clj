(ns puzzle.point)

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

; Calculate the angle of the line segment between two points
(defn angle [p1 p2]
  (let [
    x1 (x p1)
    x2 (x p2)
    y1 (y p1)
    y2 (y p2)
    dx (- x2 x1)
    dy (- y2 y1)]
    (* (/ 180 Math/PI) (Math/atan2 dy dx))))