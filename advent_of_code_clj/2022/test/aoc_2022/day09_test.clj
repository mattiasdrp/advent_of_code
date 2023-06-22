(ns aoc-2022.day09-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day09 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 13]
    (is (= expected (part-1 (resource "day09-example"))))))

(deftest part1
  (let [expected 5960]
    (is (= expected (part-1 (resource "day09"))))))

(deftest part2-example
  (let [expected 1]
    (is (= expected (part-2 (resource "day09-example"))))))

(deftest part2-example2
  (let [expected 36]
    (is (= expected (part-2 (resource "day09-example2"))))))

(deftest part2
  (let [expected 2327]
    (is (= expected (part-2 (resource "day09"))))))
