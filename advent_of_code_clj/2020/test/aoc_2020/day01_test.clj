(ns aoc-2020.day01-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2020.day01 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 514579]
    (is (= expected (part-1 (resource "day01-example"))))))

(deftest part1
  (let [expected 970816]
    (is (= expected (part-1 (resource "day01"))))))

(deftest part2-example
  (let [expected 241861950]
    (is (= expected (part-2 (resource "day01-example"))))))

(deftest part2
  (let [expected 96047280]
    (is (= expected (part-2 (resource "day01"))))))
