(ns aoc-2020.day05-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2020.day05 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 820]
    (is (= expected (part-1 (resource "day05-example"))))))

(deftest part1
  (let [expected 930]
    (is (= expected (part-1 (resource "day05"))))))

(deftest part2
  (let [expected 515]
    (is (= expected (part-2 (resource "day05"))))))
