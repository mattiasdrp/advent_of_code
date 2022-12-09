(ns aoc-2022.day08-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day08 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 21]
    (is (= expected (part-1 (resource "day08-example"))))))

(deftest part1
  (let [expected 1823]
    (is (= expected (part-1 (resource "day08"))))))

(deftest part2-example
  (let [expected 8]
    (is (= expected (part-2 (resource "day08-example"))))))

(deftest part2
  (let [expected 211680]
    (is (= expected (part-2 (resource "day08"))))))
