(ns aoc-2022.day20-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day20 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 3]
    (is (= expected (part-1 (resource "day20-example"))))))

(deftest part1
  (let [expected 4151]
    (is (= expected (part-1 (resource "day20"))))))

(deftest part2-example
  (let [expected 1623178306]
    (is (= expected (part-2 (resource "day20-example"))))))

(deftest part2
  (let [expected 7848878698663]
    (is (= expected (part-2 (resource "day20"))))))
