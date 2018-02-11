(ns puzzle.point-test
  (:require [clojure.test :refer :all]
            [puzzle.point :refer :all]))

(deftest x-test
  (testing "x"
    (is (= 42.0 (x '(42 24))))))

(deftest y-test
  (testing "y"
    (is (= 24.0 (y '(42 24))))))

(deftest dist-test
  (testing "pointdist"
    (is (= 1.0 (pointdist '(1 0) '(0 0) )))
    (is (= 1.0 (pointdist '(0 0) '(0 1) )))
    (is (= (int (Math/sqrt 200)) (int (pointdist '(0 0) '(10 10) ))))
    ))

(deftest angle-test
  (testing "angle"
    (is (= 90.0 (angle '(0 0) '(0 10))))
    (is (= -90.0 (angle '(0 10) '(0 0))))
    ))
