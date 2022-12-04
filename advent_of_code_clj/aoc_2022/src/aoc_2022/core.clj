(ns aoc-2022.core
  (:require
   [clojure.java.io :refer [resource]]
   [aoc-2022.day01]
   [aoc-2022.day02]
   [aoc-2022.day03]
   [aoc-2022.day04]
   ;; [aoc-2022.day05]
   ))

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
    ;; "d05.p1" (println (aoc-2022.day05/part-1 (resource "day05")))
    ;; "d05.p2" (println (aoc-2022.day05/part-2 (resource "day05")))
    (println (str part " not found"))))
