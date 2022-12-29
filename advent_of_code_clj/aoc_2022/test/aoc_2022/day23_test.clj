(ns aoc-2022.day23-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day23 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 110]
    (is (= expected (part-1 (resource "day23-example"))))))

(deftest part1
  (let [expected 3966]
    (is (= expected (part-1 (resource "day23"))))))

(deftest part2-example
  (let [expected 20]
    (is (= expected (part-2 (resource "day23-example"))))))

(deftest part2
  (let [expected 933]
    (is (= expected (part-2 (resource "day23"))))))
