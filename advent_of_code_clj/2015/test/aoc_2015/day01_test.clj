(ns aoc-2015.day01-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2015.day01 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1
  (let [expected 138]
    (is (= expected (part-1 (resource "day01"))))))

(deftest part2
  (let [expected 1771]
    (is (= expected (part-2 (resource "day01"))))))
