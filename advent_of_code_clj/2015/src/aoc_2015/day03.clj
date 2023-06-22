(ns aoc-2015.day03)

(defn- add-to-set [i j set]
  (conj set [i j]))

(defn- move [[set i j] c]
  (let [[i j]
        (case c
          \> (list (inc i) j)
          \< (list (dec i) j)
          \^ (list i (inc j))
          \v (list i (dec j))
          (list i j))]
    [(add-to-set i j set) i j]))

(defn part-1 [file]
  (->> (slurp file)
       seq
       (reduce move [#{[0 0]} 0 0])
       first
       count))

#_(part-1 (clojure.java.io/resource "day03-example"))
#_(part-1 (clojure.java.io/resource "day03"))

(defn- move2 [[set i j i2 j2] c]
  (let [[i j]
        (case c
          \> (list (inc i) j)
          \< (list (dec i) j)
          \^ (list i (inc j))
          \v (list i (dec j))
          (list i j))]
    [(add-to-set i j set) i2 j2 i j]))

(defn part-2 [file]
  (->> (slurp file)
       seq
       (reduce move2 [#{[0 0]} 0 0 0 0])
       first
       count))

#_(part-2 (clojure.java.io/resource "day03-example"))
#_(part-2 (clojure.java.io/resource "day03"))
