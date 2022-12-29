(ns aoc-2022.day22-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day22 :refer [part-1]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 6032]
    (is (= expected (part-1 (resource "day22-example"))))))

(deftest part1
  (let [expected 1484]
    (is (= expected (part-1 (resource "day22"))))))

;; (deftest part2-example
;;   (let [expected 93]
;;     (is (= expected (part-2 (resource "day22-example"))))))

;; (deftest part2
;;   (let [expected 29044]
;;     (is (= expected (part-2 (resource "day22"))))))
