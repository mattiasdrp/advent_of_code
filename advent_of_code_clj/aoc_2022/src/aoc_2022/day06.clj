(ns aoc-2022.day06)

(defn- find-firstn [line n]
  (loop [[_ & rest :as l] (seq line)
         index n]
    (if (= n (count (set (take n l))))
      index
      (recur rest (inc index)))))

(defn part-1 [file]
  (find-firstn (slurp file) 4))

(defn part-2 [file]
  (find-firstn (slurp file) 14))

#_(part-1 (clojure.java.io/resource "day06"))
#_(part-2 (clojure.java.io/resource "day06"))
