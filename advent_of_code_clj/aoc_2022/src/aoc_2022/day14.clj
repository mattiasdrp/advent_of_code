(ns aoc-2022.day14
  (:require
   [aoc-utils.core :as utils]
   [clojure.math :as math]))

(defn signum [x] (long (math/signum x)))

(defn- line->rock [[mset maxy] line]
  (let [coords (re-seq #"(\d+),(\d+)" line)
        [[_ px py] & rest]  coords
        px (parse-long px)
        py (parse-long py)
        maxy (max py maxy)

        [mset maxy _]
        (reduce
         (fn [[mset maxy [px py]] [_ x y]]
           (let [x (parse-long x)
                 y (parse-long y)
                 maxy (max y maxy)
                 xyseq
                 (if (= px x)
                   (let [sy (signum (- py y))
                         end (+ py sy)]
                     (range y end sy))
                   (let [sx (signum (- px x))
                         end (+ px sx)]
                     (range x end sx)))
                 mset (if (= px x)
                        (reduce (fn [mset y] (conj mset [px y])) mset xyseq)
                        (reduce (fn [mset x] (conj mset [x py])) mset xyseq))]
             [mset maxy [x y]])) [mset maxy [px py]] rest)]
    [mset maxy]))

(defn- simulate-fall [mset maxy pred]
  (loop [x 500
         y 0]
    (cond
      ;; if we reached the predicate, stop
      ;; for part 1, this is when the sand falls towards the maximum rock
      ;; for part 2, this is when the origin is blocked
      (pred mset y)
      nil

      ;; the next y is the floor, just stop where we are
      (= (inc y) maxy)
      (conj mset [x y])

      ;; the cell below is empty
      (not (contains? mset [x (inc y)]))
      (recur x (inc y))

      ;; the cell to the left is empty
      (not (contains? mset [(dec x) (inc y)]))
      (recur (dec x) (inc y))

      ;; the cell to the right is empty
      (not (contains? mset [(inc x) (inc y)]))
      (recur (inc x) (inc y))

      :else
      (conj mset [x y]))))

(defn part [mset maxy pred]
  (loop [mset mset
         cpt 0]
    (let [mset (simulate-fall mset maxy pred)]
      (cond
        (nil? mset)
        cpt

        :else
        (recur mset (inc cpt))))))

(defn part-1 [file]
  (let [[mset maxy] (utils/reduce-file line->rock [#{} 0] file)]
    (part mset (+ 2 maxy) (fn [_ y] (> y maxy)))))

(defn part-2 [file]
  (let [[mset maxy] (utils/reduce-file line->rock [#{} 0] file)]
    (part mset (+ 2 maxy) (fn [mset _] (contains? mset [500 0])))))

#_(part-1 (clojure.java.io/resource "day14"))
#_(part-2 (clojure.java.io/resource "day14"))
