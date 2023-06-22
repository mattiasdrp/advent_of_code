(ns aoc-2015.day03-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2015.day03 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 2]
    (is (= expected (part-1 (resource "day03-example"))))))

(deftest part1
  (let [expected 2565]
    (is (= expected (part-1 (resource "day03"))))))

(deftest part2-example
  (let [expected 3]
    (is (= expected (part-2 (resource "day03-example"))))))

(deftest part2
  (let [expected 2639]
    (is (= expected (part-2 (resource "day03"))))))
