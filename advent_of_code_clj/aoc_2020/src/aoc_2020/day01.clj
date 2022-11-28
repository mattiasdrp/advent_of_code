(ns aoc-2020.day01
  (:require
   [aoc-utils.core :as utils]
   [clojure.math.combinatorics :as comb]))

(defn- to-int-seq [file]
  (utils/do-all-file file read-string))

#_(to-int-seq (clojure.java.io/resource "day01"))

(defn- part
  "Takes `expected`, `n` and an `file`
  Parse the content of the `file` as a sequence of numbers and generates all the `n` combinations of these numbers
  Filters the combinations that, when added, equal the `expected` value and returns their product"
  [expected n file]
  (as-> file $
    (to-int-seq $)
    (comb/combinations $ n)
    (filter #(= expected (apply + %)) $)
    (apply * (first $))))

#_(= (part 2020 2 "1995 100 25") 49875)
#_(= (part 2020 3 "1995 100 20 5") 199500)

(defn part-1 [file]
  (part 2020 2 file))

(defn part-2 [file]
  (part 2020 3 file))
