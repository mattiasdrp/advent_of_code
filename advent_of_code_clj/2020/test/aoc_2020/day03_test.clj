(ns aoc-2020.day03-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2020.day03 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 7]
    (is (= expected (part-1 (resource "day03-example"))))))

(deftest part1
  (let [expected 184]
    (is (= expected (part-1 (resource "day03"))))))

(deftest part2-example
  (let [expected 336]
    (is (= expected (part-2 (resource "day03-example"))))))

(deftest part2
  (let [expected 2431272960]
    (is (= expected (part-2 (resource "day03"))))))
