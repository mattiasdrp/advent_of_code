(ns aoc-2022.day13
  (:require
   [clojure.java.io :refer [reader]]
   [aoc-utils.core :as utils]))

(defn- lvec [v]
  (if (sequential? v) v
      (vector v)))

(defn- lcompare [l1 l2]
  (cond
    (and (nil? l1) (nil? l2))
    0

    (nil? l1)
    -1

    (nil? l2)
    1

    (and (int? l1) (int? l2))
    (compare l1 l2)

    (and (sequential? l1) (sequential? l2))
    (let [[hd1 & tl1] l1
          [hd2 & tl2] l2
          c (lcompare hd1 hd2)]
      (if (= 0 c) (lcompare tl1 tl2)
          c))

    :else
    (lcompare (lvec l1) (lvec l2))))

#_(lcompare [1 1 1 1] [1 1 1 1])
(defn part-1 [file]
  (with-open [rdr (reader file)]
    (loop [[l1 l2 _ & rest] (line-seq rdr)
           index 1
           correct 0]
      (cond
        (nil? l1)
        correct

        :else
        (let [l1 (read-string l1)
              l2 (read-string l2)]
          (recur rest (inc index) (if (< (lcompare l1 l2) 0) (+ correct index) correct)))))))

(defn part-2 [file]
  (let [list (utils/reduce-file
              (fn [acc l]
                (if (empty? l) acc
                    (conj acc (read-string l)))) '() file)
        [pos2 pos6] (reduce (fn [[pos2 pos6] v]
                              (if (< (lcompare v [[2]]) 0)
                                [(inc pos2) (inc pos6)]
                                (if (< (lcompare v [[6]]) 0)
                                  [pos2 (inc pos6)]
                                  [pos2 pos6]))) [1 2] list)
        indices (* pos2 pos6)]
    indices))

#_(part-1 (clojure.java.io/resource "day13"))
#_(part-2 (clojure.java.io/resource "day13"))
