(ns aoc-2022.day21-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day21 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 152]
    (is (= expected (part-1 (resource "day21-example"))))))

(deftest part1
  (let [expected 331120084396440]
    (is (= expected (part-1 (resource "day21"))))))

(deftest part2-example
  (let [expected 301]
    (is (= expected (part-2 (resource "day21-example"))))))

(deftest part2
  (let [expected 3378273370680]
    (is (= expected (part-2 (resource "day21"))))))
