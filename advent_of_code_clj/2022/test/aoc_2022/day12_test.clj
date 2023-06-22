(ns aoc-2022.day12-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day12 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 31]
    (is (= expected (part-1 (resource "day12-example"))))))

(deftest part1
  (let [expected 447]
    (is (= expected (part-1 (resource "day12"))))))

(deftest part2-example
  (let [expected 29]
    (is (= expected (part-2 (resource "day12-example"))))))

(deftest part2
  (let [expected 446]
    (is (= expected (part-2 (resource "day12"))))))
