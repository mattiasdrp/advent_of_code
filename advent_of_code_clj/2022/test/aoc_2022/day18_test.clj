(ns aoc-2022.day18-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day18 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 64]
    (is (= expected (part-1 (resource "day18-example"))))))

(deftest part1
  (let [expected 4456]
    (is (= expected (part-1 (resource "day18"))))))

(deftest part2-example
  (let [expected 58]
    (is (= expected (part-2 (resource "day18-example"))))))

(deftest part2
  (let [expected 2510]
    (is (= expected (part-2 (resource "day18"))))))
