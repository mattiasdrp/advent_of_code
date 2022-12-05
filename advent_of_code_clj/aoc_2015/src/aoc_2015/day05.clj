(ns aoc-2015.day05
  (:require
   [aoc-utils.core :as utils]))

(def vocals #{\a \e \i \o \u})

(def forbidden #{"ab" "cd" "pq" "xy"})

(defn- check [acc line]
  (loop [[hd & tl] (seq line)
         vcls 0
         double false
         prev nil]
    (cond
      (nil? hd)
      (if (and double (>= vcls 3))
        (inc acc)
        acc)

      :else
      (if (contains? forbidden (str prev hd))
        acc
        (let [vcls (if (contains? vocals hd) (inc vcls) vcls)
              double (or double (= prev hd))]
          (recur tl vcls double hd))))))

(defn part-1 [file]
  (utils/reduce-file check 0 file))

#_(part-1 (clojure.java.io/resource "day05"))

(defn- new-check [acc line]
  (loop [[hd1 & [hd2 & _ :as tl]] (seq line)
         double {}
         prev nil
         index 0
         double_pair false
         one_repeat false]
    (cond
      (nil? hd1)
      (if (and double_pair one_repeat) (inc acc) acc)

      :else
      (let [pair (str hd1 hd2)
            indexp (get double pair)
            double (if (nil? indexp) (assoc double pair index) double)
            one_repeat (or one_repeat (= prev hd2))
            double_pair (or double_pair (and (some? indexp) (> index (inc indexp))))]

        (recur tl double hd1 (inc index) double_pair one_repeat)))))
      ;;       id' (pair double)]

(defn part-2 [file]
  (utils/reduce-file new-check 0 file))

#_(part-2 (clojure.java.io/resource "day05-example"))
