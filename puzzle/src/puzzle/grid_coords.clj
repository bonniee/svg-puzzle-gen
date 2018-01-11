; Returns coordinates for a squiggly puzzle.

(ns puzzle.grid-coords)

; TODO parameterize the size / spacing better.

; Sets up coordinates for puzzle piece anchor points
(defn seed-coords [N & {:keys [jitter] :or {jitter 50}}]
  (set (for [x (range N) y (range N)]
    (let [xcoord (+ 50 (* 100 x))
          ycoord (+ 50 (* 100 y))
          xjitter (rand-int jitter)
          yjitter (rand-int jitter)
      ] (vec (list (+ xcoord xjitter) (+ ycoord yjitter)))))))