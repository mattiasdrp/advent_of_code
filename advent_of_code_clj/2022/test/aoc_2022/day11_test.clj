(ns aoc-2022.day11-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day11 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 10605]
    (is (= expected (part-1 (resource "day11-example"))))))

(deftest part1
  (let [expected 113220]
    (is (= expected (part-1 (resource "day11"))))))

(deftest part2-example
  (let [expected 2713310158]
    (is (= expected (part-2 (resource "day11-example"))))))

(deftest part2
  (let [expected 30599555965]
    (is (= expected (part-2 (resource "day11"))))))
