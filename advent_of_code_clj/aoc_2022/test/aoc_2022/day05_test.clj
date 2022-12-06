(ns aoc-2022.day05-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day05 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected "CMZ"]
    (is (= expected (part-1 (resource "day05-example"))))))

(deftest part1
  (let [expected "VCTFTJQCG"]
    (is (= expected (part-1 (resource "day05"))))))

(deftest part2-example
  (let [expected "MCD"]
    (is (= expected (part-2 (resource "day05-example"))))))

(deftest part2
  (let [expected "GCFGLDNJZ"]
    (is (= expected (part-2 (resource "day05"))))))
