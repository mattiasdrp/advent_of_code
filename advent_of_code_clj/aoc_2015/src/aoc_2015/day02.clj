(ns aoc-2015.day02
  (:require [aoc-utils.core :as utils]
            [clojure.string :as str]))

(defn- surface [l w h]
  (* 2 (+ (* l w) (* l h) (* w h))))

(defn- area-smaller [l w h]
  (->> (list l w h)
       sort
       (take 2)
       (apply *)))

(defn part-1 [file]
  (utils/reduce-file (fn [acc line]
                       (let [[l w h] (str/split line #"x")
                             l (read-string l)
                             w (read-string w)
                             h (read-string h)]
                         (+ acc (surface l w h) (area-smaller l w h)))) 0 file))

(defn- perimeter-smaller [l w h]
  (->> (list l w h)
       sort
       (take 2)
       (reduce #(+ %1 (* 2 %2)) 0)))

(defn part-2 [file]
  (utils/reduce-file (fn [acc line]
                       (let [[l w h] (str/split line #"x")
                             l (read-string l)
                             w (read-string w)
                             h (read-string h)]
                         (+ acc (* l w h) (perimeter-smaller l w h)))) 0 file))
