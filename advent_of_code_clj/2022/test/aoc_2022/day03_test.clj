(ns aoc-2022.day03-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day03 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 157]
    (is (= expected (part-1 (resource "day03-example"))))))

(deftest part1
  (let [expected 7674]
    (is (= expected (part-1 (resource "day03"))))))

(deftest part2-example
  (let [expected 70]
    (is (= expected (part-2 (resource "day03-example"))))))

(deftest part2
  (let [expected 2805]
    (is (= expected (part-2 (resource "day03"))))))
