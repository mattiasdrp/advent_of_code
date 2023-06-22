(ns aoc-2022.day20
  (:require [aoc-utils.core :as utils]))

(defn- index-of [ring cell]
  (first (keep-indexed (fn [id cell'] (when (= cell cell') id)) ring)))

(defn- change-position [[v _ :as cell] ring length]
  (let [id-from (index-of ring cell)
        id-to (mod (+ id-from v) (dec length))]
    (cond

      ;; [... (f) ... (t) ...]
      (<= id-from id-to)
      (-> []
          (into (subvec ring 0 id-from))
          (into (subvec ring (inc id-from) (inc id-to)))
          (conj cell)
          (into (subvec ring (inc id-to))))

      ;; [... (t) ... (t) ...]
      (<= id-to id-from)
      (-> []
          (into (subvec ring 0 id-to))
          (conj cell)
          (into (subvec ring id-to id-from))
          (into (subvec ring (inc id-from))))

      ;; [... (t=f) ...]
      :else ring)))

#_(change-position [3 3] [[0 0] [1 1] [2 2] [3 3] [4 4]] 5)
#_(change-position [1 1] [[0 0] [1 1] [2 2] [3 3] [4 4]] 5)
#_(change-position [2 2] [[0 0] [1 1] [2 2] [3 3] [4 4]] 5)
#_(change-position [-3 3] [[0 0] [1 1] [2 2] [-3 3] [4 4]] 5)

(defn- permut-array [numbers ring times length]
  (loop [times times
         ring ring]
    (cond
      (= times 0)
      ring

      :else
      (recur (dec times)
             (reduce
              (fn [ring cell] (change-position cell ring length))
              ring numbers)))))

(defn- prepare-list [file key]
  (utils/reduce-file
   (fn [[acc ring id] l]
     (let [n (parse-long l)
           n (* key n)]
       (if (= n 0)
         [(conj acc [n 0]) (conj ring [n 0]) id]
         [(conj acc [n id]) (conj ring [n id]) (inc id)]))) [[] [] 1] file))

(defn- part [file key times]
  (let [[numbers ring length] (prepare-list file key)
        ring (into [] (permut-array numbers ring times length))
        id0 (doto (index-of ring [0 0]) prn)
        v1000 (first (get ring (mod (+ id0 1000) length)))
        v2000 (first (get ring (mod (+ id0 2000) length)))
        v3000 (first (get ring (mod (+ id0 3000) length)))]
    (doto (+ v1000 v2000 v3000) prn)))

(defn part-1 [file] (part file 1 1))

(defn part-2 [file] (part file 811589153 10))
#_(part-1 (clojure.java.io/resource "day20"))
#_(part-2 (clojure.java.io/resource "day20-example"))

#_(split-at 3 [1 2 3 4 5])
