(ns aoc-2022.core
  (:require
   [clojure.java.io :refer [resource]]
   [aoc-2022.day01]
   [aoc-2022.day02]
   [aoc-2022.day03]
   [aoc-2022.day04]
   [aoc-2022.day05]
   [aoc-2022.day06]
   [aoc-2022.day07]
   [aoc-2022.day08]
   [aoc-2022.day09]
   [aoc-2022.day10]
   [aoc-2022.day11]
   [aoc-2022.day12]
   [aoc-2022.day13]
   [aoc-2022.day14]
   [aoc-2022.day15]
   [aoc-2022.day16]
   [aoc-2022.day17]))

(defn -main [part]
  (case part
    "d01.p1" (println (aoc-2022.day01/part-1 (resource "day01")))
    "d01.p2" (println (aoc-2022.day01/part-2 (resource "day01")))
    "d02.p1" (println (aoc-2022.day02/part-1 (resource "day02")))
    "d02.p2" (println (aoc-2022.day02/part-2 (resource "day02")))
    "d03.p1" (println (aoc-2022.day03/part-1 (resource "day03")))
    "d03.p2" (println (aoc-2022.day03/part-2 (resource "day03")))
    "d04.p1" (println (aoc-2022.day04/part-1 (resource "day04")))
    "d04.p2" (println (aoc-2022.day04/part-2 (resource "day04")))
    "d05.p1" (println (aoc-2022.day05/part-1 (resource "day05")))
    "d05.p2" (println (aoc-2022.day05/part-2 (resource "day05")))
    "d06.p1" (println (aoc-2022.day06/part-1 (resource "day06")))
    "d06.p2" (println (aoc-2022.day06/part-2 (resource "day06")))
    "d07.p1" (println (aoc-2022.day07/part-1 (resource "day07")))
    "d07.p2" (println (aoc-2022.day07/part-2 (resource "day07")))
    "d08.p1" (println (aoc-2022.day08/part-1 (resource "day08")))
    "d08.p2" (println (aoc-2022.day08/part-2 (resource "day08")))
    "d09.p1" (println (aoc-2022.day09/part-1 (resource "day09")))
    "d09.p2" (println (aoc-2022.day09/part-2 (resource "day09")))
    "d10.p1" (println (aoc-2022.day10/part-1 (resource "day10")))
    "d10.p2" (println (aoc-2022.day10/part-2 (resource "day10")))
    "d11.p1" (println (aoc-2022.day11/part-1 (resource "day11")))
    "d11.p2" (println (aoc-2022.day11/part-2 (resource "day11")))
    "d12.p1" (println (aoc-2022.day12/part-1 (resource "day12")))
    "d12.p2" (println (aoc-2022.day12/part-2 (resource "day12")))
    "d13.p1" (println (aoc-2022.day13/part-1 (resource "day13")))
    "d13.p2" (println (aoc-2022.day13/part-2 (resource "day13")))
    "d14.p1" (println (aoc-2022.day14/part-1 (resource "day14")))
    "d14.p2" (println (aoc-2022.day14/part-2 (resource "day14")))
    "d16.p1" (println (aoc-2022.day16/part-1 (resource "day16")))
    "d16.p2" (println (aoc-2022.day16/part-2 (resource "day16")))
    "d15.p1" (println (aoc-2022.day15/part-1 (resource "day15") 2000000))
    "d15.p2" (println (aoc-2022.day15/part-2 (resource "day15")))
    "d17.p1" (println (aoc-2022.day17/part-1 (resource "day17") 50))
    "d17.p2" (println (aoc-2022.day17/part-2 (resource "day17") 50))
    (println (str part " not found"))))
