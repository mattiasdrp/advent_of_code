(ns aoc-2022.day03
  (:require [aoc-utils.core :as utils]
            [clojure.set :as set]
            [clojure.java.io :refer [reader]]))

(defn- priority [elem]
  (cond
    (<= (int \A) elem (int \Z)) (+ 27 (- elem (int \A)))
    (<= (int \a) elem (int \z)) (inc (- elem (int \a)))))

(defn- split-and-disj [acc line]
  (let [c (/ (count line) 2)
        p1 (into #{} (subs line 0 c))
        p2 (into #{} (subs line c))
        elem (int (first (set/intersection p1 p2)))]
    (+ acc (priority elem))))

(defn part-1 [file]
  (utils/reduce-file split-and-disj 0 file))

#_(split-and-disj "vJrwpWtwJgWrhcsFMMfFFhFp")
#_(part-1 (clojure.java.io/resource "day03-example"))

(defn part-2 [file]
  (with-open [rdr (reader file)]
    (let [seq (line-seq rdr)]
      (loop [acc 0
             [l1 l2 l3 & tl] seq]
        (if (nil? l1) acc
            (let [l1 (into #{} l1)
                  l2 (into #{} l2)
                  l3 (into #{} l3)
                  elem (int (first (set/intersection l1 l2 l3)))]
              (recur (+ acc (priority elem)) tl)))))))

#_(part-2 (clojure.java.io/resource "day03-example"))
