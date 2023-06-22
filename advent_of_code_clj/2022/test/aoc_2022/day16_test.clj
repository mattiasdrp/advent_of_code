(ns aoc-2022.day16-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day16 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 1651]
    (is (= expected (part-1 (resource "day16-example"))))))

(deftest part1
  (let [expected 1638]
    (is (= expected (part-1 (resource "day16"))))))

(deftest part2-example
  (let [expected 1707]
    (is (= expected (part-2 (resource "day16-example"))))))

(deftest part2
  (let [expected 2400]
    (is (= expected (part-2 (resource "day16"))))))
