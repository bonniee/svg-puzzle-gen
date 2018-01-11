(ns puzzle.circular-coords)

(def origin-x 600)
(def origin-y 600)
(def jitter 40)

(defn- xlen [angle radius]
  (* radius (Math/cos (Math/toRadians angle))))

(defn- ylen [angle radius]
  (* radius (Math/sin (Math/toRadians angle))))

(defn- anglediff [slices] (/ 360.0 slices))

(defn- coord-for-angle [angle radius]
  [(+ (rand-int jitter) (+ origin-x (xlen angle radius)))
   (+ (rand-int jitter) (+ origin-y (ylen angle radius)))])

(defn- mod-angle [angle]
  (mod angle 360))

(defn- angles[N offset]
  (for [x (range N)] (+ offset (mod-angle (* x (anglediff N))))))

(defn- circle-coords [N r] (map #(coord-for-angle % r) (angles N (rand-int 360))))

(defn- slices-for-ring [ring]
  (+ (rand-int 2) ring))

(defn seed-coords [rings]
  (concat (reduce concat (for [ring (range rings)]
      (circle-coords (slices-for-ring ring) (+ 2 (* 50 ring)))))
    [[origin-x origin-y]]))
