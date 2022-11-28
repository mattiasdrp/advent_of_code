(ns aoc-2020.day05
  (:require [aoc-utils.core :as utils]))

;; low and high will always have a different parity hence the + 1
(defn- split [{:keys [low high] :as acc} c]
  (cond
    (or (= c \F) (= c \L)) (update acc
                                   :high (fn [_] (dec (/ (+ low high 1) 2))))
    (or (= c \B) (= c \R))  (update acc
                                    :low (fn [_] (/ (+ low high 1) 2)))))

#_(split {:low 0 :high 127} \F)
#_(split (split {:low 0 :high 127} \F) \B)
#_(split {:low 0 :high 127} \B)

(defn- simplify [l high]
  (loop [[hd & tl] l,
         acc {:low 0, :high high}]
    (cond
      (nil? hd) (:low acc)
      :else (recur tl (split acc hd)))))

#_(simplify '(\F \B \F \B \B \F \F) 127)

(defn- id [pass]
  (let [row (take 7 pass)
        col (drop 7 pass)]
    (+ (* 8 (simplify row 127)) (simplify col 7))))

#_(id "FBFBBFFRLR")

(defn part-1 [file]
  (utils/reduce-file file (fn [acc line] (max acc (id line))) 0))

#_(part-1 (clojure.java.io/resource "day05-example"))

(defn- find-gap [[hd & tl]]
  (loop [prev hd,
         [hd & tl] tl]
    (cond
      (nil? hd) (prn "error")
      (= (inc prev) hd) (recur hd tl)
      :else (dec hd))))

#_(find-gap '(1 2 3 4 6 7))

(defn part-2 [file]
  (as->
   (utils/reduce-file file (fn [acc line] (conj acc (id line))) #{}) set
    (sort set)
    (find-gap set)))

#_(part-2 (clojure.java.io/resource "day05"))
