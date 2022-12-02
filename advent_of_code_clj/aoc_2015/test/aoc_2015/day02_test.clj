(ns aoc-2015.day02-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2015.day02 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 101]
    (is (= expected (part-1 (resource "day02-example"))))))

(deftest part1
  (let [expected 1598415]
    (is (= expected (part-1 (resource "day02"))))))

(deftest part2-example
  (let [expected 48]
    (is (= expected (part-2 (resource "day02-example"))))))

(deftest part2
  (let [expected 3812909]
    (is (= expected (part-2 (resource "day02"))))))
