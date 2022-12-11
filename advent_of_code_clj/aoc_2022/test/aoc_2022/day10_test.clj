(ns aoc-2022.day10-test
  (:require [clojure.test :refer [deftest is]]
            [aoc-2022.day10 :refer [part-1 part-2]]
            [clojure.java.io :refer [resource]]))

(deftest part1-example
  (let [expected 13140]
    (is (= expected (part-1 (resource "day10-example"))))))

(deftest part1
  (let [expected 17380]
    (is (= expected (part-1 (resource "day10"))))))

(deftest part2-example
  (let [expected "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
"]
    (is (= expected (with-out-str (part-2 (resource "day10-example")))))))

(deftest part2
  (let [expected "####..##...##..#..#.####.###..####..##..
#....#..#.#..#.#..#....#.#..#.#....#..#.
###..#....#....#..#...#..#..#.###..#....
#....#.##.#....#..#..#...###..#....#....
#....#..#.#..#.#..#.#....#.#..#....#..#.
#.....###..##...##..####.#..#.####..##..
"]
    (is (= expected (with-out-str (part-2 (resource "day10")))))))
