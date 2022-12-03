(ns aoc-2015.day04-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2015.day04 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example1
  (let [expected 609043]
    (is (= expected (part-1 (resource "day04-example1"))))))

(deftest part1-example2
  (let [expected 1048970]
    (is (= expected (part-1 (resource "day04-example2"))))))

(deftest part1
  (let [expected 346386]
    (is (= expected (part-1 (resource "day04"))))))

(deftest part2
  (let [expected 9958218]
    (is (= expected (part-2 (resource "day04"))))))
