(ns aoc-2022.day14-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day14 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 24]
    (is (= expected (part-1 (resource "day14-example"))))))

(deftest part1
  (let [expected 979]
    (is (= expected (part-1 (resource "day14"))))))

(deftest part2-example
  (let [expected 93]
    (is (= expected (part-2 (resource "day14-example"))))))

(deftest part2
  (let [expected 29044]
    (is (= expected (part-2 (resource "day14"))))))
