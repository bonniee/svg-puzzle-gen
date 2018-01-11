(ns puzzle.circular)

(defn- xlen [angle radius]
  (Math/cos (Math/toRadians angle)))

(defn- ylen [angle radius]
  (Math/sin (Math/toRadians angle)))

(defn- anglediff [slices] (/ 360.0 slices))

(defn- coord-for-angle [angle radius]
  [(xlen angle radius) (ylen angle radius)])

(defn- mod-angle [angle]
  (mod angle 360))

(defn- angles[N offset]
  (for [x (range N)] (+ offset (mod-angle (* x (anglediff N))))))

(defn- circle-coords [N r] (map #(coord-for-angle % r) (angles N (rand-int 360))))

(defn coords [rings]
  (concat (reduce concat (for [ring (range rings)]
      (circle-coords (+ (rand-int 3) 8) (+ 1 (* 100 ring)))))
    [[0 0]]))
