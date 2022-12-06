(ns aoc-2022.day07-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day07 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 95437]
    (is (= expected (part-1 (resource "day07-example"))))))

(deftest part1
  (let [expected 1642503]
    (is (= expected (part-1 (resource "day07"))))))

(deftest part2-example
  (let [expected 24933642]
    (is (= expected (part-2 (resource "day07-example"))))))

(deftest part2
  (let [expected 6999588]
    (is (= expected (part-2 (resource "day07"))))))
