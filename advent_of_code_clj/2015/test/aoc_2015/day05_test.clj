(ns aoc-2015.day05-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2015.day05 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

;; These tests take too long

(deftest part1-example
  (let [expected 2]
    (is (= expected (part-1 (resource "day05-example"))))))

(deftest part1
  (let [expected 258]
    (is (= expected (part-1 (resource "day05"))))))

(deftest part2-example
  (let [expected 0]
    (is (= expected (part-2 (resource "day05-example"))))))

(deftest part2
  (let [expected 53]
    (is (= expected (part-2 (resource "day05"))))))
