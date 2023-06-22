(ns aoc-2022.day13-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day13 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 13]
    (is (= expected (part-1 (resource "day13-example"))))))

(deftest part1
  (let [expected 4894]
    (is (= expected (part-1 (resource "day13"))))))

(deftest part2-example
  (let [expected 140]
    (is (= expected (part-2 (resource "day13-example"))))))

(deftest part2
  (let [expected 24180]
    (is (= expected (part-2 (resource "day13"))))))
