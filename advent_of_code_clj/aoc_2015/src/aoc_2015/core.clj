(ns aoc-2015.core
  (:require
   [clojure.java.io :refer [resource]]
   [aoc-2015.day01]
   [aoc-2015.day02]))

(defn -main [part]
  (case part
    "d01.p1" (println (aoc-2015.day01/part-1 (resource "day01")))
    "d01.p2" (println (aoc-2015.day01/part-2 (resource "day01")))
    "d02.p1" (println (aoc-2015.day02/part-1 (resource "day02")))
    "d02.p2" (println (aoc-2015.day02/part-2 (resource "day02")))
    (println (str part " not found"))))
