(ns aoc-2022.day01-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day01 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 24000]
    (is (= expected (part-1 (resource "day01-example"))))))

(deftest part1
  (let [expected 74711]
    (is (= expected (part-1 (resource "day01"))))))

(deftest part2-example
  (let [expected 45000]
    (is (= expected (part-2 (resource "day01-example"))))))

(deftest part2
  (let [expected 209481]
    (is (= expected (part-2 (resource "day01"))))))
