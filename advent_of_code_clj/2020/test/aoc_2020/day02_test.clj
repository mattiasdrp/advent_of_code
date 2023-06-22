(ns aoc-2020.day02-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2020.day02 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 2]
    (is (= expected (part-1 (resource "day02-example"))))))

(deftest part1
  (let [expected 465]
    (is (= expected (part-1 (resource "day02"))))))

(deftest part2-example
  (let [expected 1]
    (is (= expected (part-2 (resource "day02-example"))))))

(deftest part2
  (let [expected 294]
    (is (= expected (part-2 (resource "day02"))))))
