(ns aoc-2022.day04
  (:require [aoc-utils.core :as utils]))

(defn- parse [line]
  (let [[_ & rest] (re-find #"(\d+)-(\d+),(\d+)-(\d+)" line)]
    (map read-string rest)))

(defn- cover [acc line]
  (let [[low1 high1 low2 high2] (parse line)]
    (if (or (<= low1 low2 high2 high1)
            (<= low2 low1 high1 high2))
      (inc acc)
      acc)))

(defn part-1 [file]
  (utils/reduce-file cover 0 file))

#_(part-1 (clojure.java.io/resource "day04"))

(defn- overlap [acc line]
  (let [[low1  high1 low2 high2] (parse line)]
    (if (<= (max low1 low2) (min high1 high2))
      (inc acc)
      acc)))

(defn part-2 [file]
  (utils/reduce-file overlap 0 file))

#_(part-2 (clojure.java.io/resource "day04"))
