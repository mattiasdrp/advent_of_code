(ns aoc-2022.day01
  (:require [aoc-utils.core :as utils]))

(defn- parse-or-return
  "Compute the sum of lines as long as they're not empty and stores the max of these sums"
  [{:keys [res curr_cals] :as acc} line]
  (cond
    (empty? line) {:res (max res curr_cals) :curr_cals 0}
    :else (update acc :curr_cals (fn [_] (+ curr_cals (read-string line))))))

#_(parse-or-return {:res 0 :curr_cals 500} "1000")

(defn part-1 [file]
  (:res (utils/reduce-file-newline parse-or-return {:res 0 :curr_cals 0} file)))

#_(part-1 (clojure.java.io/resource "day01-example"))

(defn- conj-3-list
  "Adds an element to a list and keeps the 3 max elements of this list"
  [e l]
  (as-> (conj l e) l
    (sort >= l)
    (take 3 l)))

#_(conj-3-list 5 '(1))

(defn- parse-or-return2
  "Compute the sum of lines as long as they're not empty and stores the 3 max of these sums"
  [{:keys [res curr_cals] :as acc} line]
  (cond
    (empty? line) {:res (conj-3-list curr_cals res) :curr_cals 0}
    :else (update acc :curr_cals (fn [_] (+ curr_cals (read-string line))))))

#_(parse-or-return2 {:res '(1000) :curr_cals 1500} "")
#_(parse-or-return2 {:res '(1000 800 600) :curr_cals 1500} "")

(defn part-2 [file]
  (as-> (utils/reduce-file-newline parse-or-return2 {:res '() :curr_cals 0} file) res
    (:res res)
    (reduce + res)))

#_(part-2 (clojure.java.io/resource "day01-example"))
