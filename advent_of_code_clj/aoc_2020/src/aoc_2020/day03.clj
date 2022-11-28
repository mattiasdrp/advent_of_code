(ns aoc-2020.day03
  (:require
   [aoc-utils.core :as utils]))

(defn- read-matrix [file]
  (->> (utils/do-all-file file identity)
       (mapv vec)))

(defn- run-down [file [xdelta ydelta]]
  (let [matrix (read-matrix file)
        height (count matrix)
        width (count (first matrix))]
    (loop [x 0, y 0, trees 0]
      (let [x (mod x width)]
        (cond
          (>= y height) trees
          :else (recur (+ x xdelta) (+ y ydelta) (if (= ((matrix y) x) \#) (inc trees) trees)))))))

(defn part-1 [file]
  (run-down file '(3 1)))

(defn part-2 [file]
  (let [deltas '((1 1) (3 1) (5 1) (7 1) (1 2))]
    (apply * (map #(run-down file %) deltas))))
