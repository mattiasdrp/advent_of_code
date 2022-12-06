(ns aoc-2022.day06-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day06 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 5]
    (is (= expected (part-1 (resource "day06-example"))))))

(deftest part1
  (let [expected 1578]
    (is (= expected (part-1 (resource "day06"))))))

(deftest part2-example
  (let [expected 23]
    (is (= expected (part-2 (resource "day06-example"))))))

(deftest part2
  (let [expected 2178]
    (is (= expected (part-2 (resource "day06"))))))
