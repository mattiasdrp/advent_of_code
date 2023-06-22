(ns aoc-2022.day17-test
  (:require
   [clojure.test :refer [deftest is]]
   [aoc-2022.day17 :refer [part-1 part-2]]
   [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 3068]
    (is (= expected (part-1 (resource "day17-example") 10)))))

(deftest part1
  (let [expected 3048]
    (is (= expected (part-1 (resource "day17") 50)))))

(deftest part2-example
  (let [expected 1514285714288]
    (is (= expected (part-2 (resource "day17-example") 10)))))

(deftest part2
  (let [expected 1504093567249]
    (is (= expected (part-2 (resource "day17") 50)))))
