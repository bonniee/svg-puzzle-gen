(ns puzzle.whimsy
  (:require [puzzle.strings :as strings])
  (:require [puzzle.point :refer :all])
  (:require [puzzle.svg :as svg])
  (:require [puzzle.squiggle :as squiggle])
  (:require [clojure.set :as set])
  (:require [kdtree]))

; Whimsy: a whimsy puzzle piece
; origin: center point for placement
; radius: radius of the clipping circle for the whimsy
; svgpath: for drawing it
; anchors: points that should be connected to the rest of the puzzle
(defrecord Whimsy [origin radius svgpath anchors])

(def cat-whimsy
  (Whimsy. [400 400] 250 strings/cat-whimsy (
      list
        [339.23 484.17] ; tail
        [384.24 386.15] ; back
        [436.84 321.39] ; left ear
        [465.03 459.87] ; forepaw
        [475.61 377.46] ; chest
            )))

(def circular-whimsy
  (Whimsy. 
    [700 800] 150
    (svg/point [700 800] :radius 50)
    (list [700 750] [700 850] [650 800] [750 800])))

(def WHIMSIES (list cat-whimsy))

(defn whimsy-paths
  "Get the concatenated svgpaths for each whimsy"
  [whimsies]
  (apply str (map (fn [w] (.-svgpath w)) whimsies)))

(defn- disallowed_points
  "Coords should be a set.
  Returns a set of points which conflict with the clipping circle around the provided point."
  [coords point radius]
  (set (filter (fn [candidate] (> radius (pointdist candidate point))) coords)))


(defn- whimsy_disallowed_points
  "Identifies coordinates which would conflict with whimsy pieces.
  Whimsies should be a collection of Whimsy"
  [coords whimsies]
  (reduce concat
    (map (fn [w] (disallowed_points coords (.-origin w) (.-radius w))) whimsies)))

(defn whimsy_coords
  "Transforms coordinate set to include whimsy origins
  and exclude any coordinates that conflict with whimsies"
  [base-coords whimsies]
  (vec
    (concat (map (fn [w] (.-origin w)) whimsies) ; TODO: is there shorthand for this? %?
    (set/difference base-coords (whimsy_disallowed_points base-coords whimsies))))
  )

(defn whimsy_anchor_paths
  "Draws puzzle-piece connections between the rest of the puzzle,
  and the anchor points on whimsy pieces.
  points represent the current points in the puzzle."
  [points whimsies]
  (let [tree (kdtree/build-tree points)]
    (flatten (map (
      fn [whimsy] (map (fn [anchor]
        (let [neighbor (:point (nth (kdtree/nearest-neighbor tree anchor 2) 0))] ; todo double check this
          (squiggle/squiggle-path-svg [anchor neighbor]))
        ) (.-anchors whimsy))
      ) whimsies))))

