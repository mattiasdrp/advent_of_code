(ns aoc-2015.day01
  (:require [aoc-utils.core :as utils]))

(defn- transform [acc c]
  (case c
    \( (inc acc)
    \) (dec acc)
    \newline acc))

(defn- iter [s]
  (reduce transform 0 (seq s)))

#_(iter "(()()")

(defn part-1 [file]
  (iter (slurp file)))

#_(seq (slurp (clojure.java.io/resource "day01")))
#_(part-1 (clojure.java.io/resource "day01"))

(defn- transform2 [[floor ind] c]
  (cond
    (= c \() [(inc floor) (inc ind)]
    (and (= floor 0) (= c \))) (reduced [floor ind])
    (= c \)) [(dec floor) (inc ind)]))

#_(transform2 [0 1] \))
(defn- iter2 [s]
  (reduce transform2 [0 1] (seq s)))

#_(iter2 "()())")

(defn part-2 [file]
  (nth (iter2 (slurp file)) 1))

#_(part-2 (clojure.java.io/resource "day01"))
