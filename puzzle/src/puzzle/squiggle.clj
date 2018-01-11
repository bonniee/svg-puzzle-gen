
(ns puzzle.squiggle
  (:require [puzzle.svg :refer :all]))

; This file is for drawing a squiggle.

(def x-start 10.0)

(def x-end 90.0)

(def dx (- x-end x-start))

(defn- control-point-height []
  (+ (rand-int 10) 20.0))

(defn- neg-control-point-height []
              (* -1 (control-point-height)))

(defn- squiggle-s-phrase [x h]
              (s-path-phrase x h x 0))

; Adds +/- jitter to base value
(defn- jitter [base jitter]
  (+ base (- jitter (rand-int jitter))))

; TODO play around with this
(defn- control-point-x-jitter [x squig-width]
  (jitter x (/ squig-width 2.0)))

(defn- first-c-phrase [squig-width]
  (let [x-end (+ squig-width x-start)]
    (c-path-phrase
          (control-point-x-jitter x-start squig-width)
          (neg-control-point-height)
          (control-point-x-jitter x-end squig-width)
          (control-point-height)
          x-end
          0)))

(defn- s-partitions [squig-width]
  (let [s-phrase-start (+ squig-width (+ squig-width x-start))]
    (take-nth squig-width (range s-phrase-start x-end))))

(defn- s-phrases [squig-width]
  (apply str
    (map-indexed (fn [i x]
      (s-path-phrase
        x
        (if (= 0 (mod i 2)) (control-point-height) (neg-control-point-height))
        x
        0
        ))
      (s-partitions squig-width))))

; Create the d attribute for a squiggle path
(defn- squiggle-path-d []
  (let [num-squigs 2
        squig-width (/ dx num-squigs)]
        (str  (m-path-phrase 0 0 x-start 0)
              (first-c-phrase squig-width)
              (s-phrases squig-width)
              (s-path-phrase x-end (control-point-height) x-end 0)
              (m-path-phrase x-end 0 100 0))))

; Creates an SVG element using a series of cubic bezier curves
(defn squiggle-path-svg [p1 p2]
  (svg-path p1 p2 (squiggle-path-d) 100.0))