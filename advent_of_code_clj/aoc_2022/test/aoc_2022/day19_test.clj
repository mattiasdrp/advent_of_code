(ns aoc-2022.day19-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day19 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 33]
    (is (= expected (part-1 (resource "day19-example"))))))

(deftest part1
  (let [expected 1624]
    (is (= expected (part-1 (resource "day19"))))))

(deftest part2-example
  (let [expected 62]
    (is (= expected (part-2 (resource "day19-example"))))))

(deftest part2
  (let [expected 12628]
    (is (= expected (part-2 (resource "day19"))))))
