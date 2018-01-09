(def x (ref 1))
(def y (ref 1))

(defn new-vals []
  (dosync
    (alter x inc)
    (ref-set y (+ 2 @x))))

(let [n 2]
  (future (dotimes [_ n] (new-vals)))
  (future (dotimes [_ n] (new-vals)))
  )
