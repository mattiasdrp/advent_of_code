(ns aoc-2020.day04-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2020.day04 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 2]
    (is (= expected (part-1 (resource "day04-example"))))))

(deftest part1
  (let [expected 210]
    (is (= expected (part-1 (resource "day04"))))))

(deftest part2-example
  (let [expected 2]
    (is (= expected (part-2 (resource "day04-example"))))))

(deftest part2
  (let [expected 131]
    (is (= expected (part-2 (resource "day04"))))))
