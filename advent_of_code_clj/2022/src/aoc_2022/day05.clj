(ns aoc-2022.day05
  (:require
   [clojure.java.io :refer [reader]]))

(defn- parse-line [crates line]
  (let [reg (re-seq #"(\s{3}|\[(\w)\])\s?" line)
        new-crates (map #(get % 2) reg)
        crates (if (empty? crates) (map (fn [_] '()) new-crates) crates)
        crates (map cons new-crates crates)]
    crates))

(defn- parse-crates [lines]
  (loop [[[_ c :as line] & lines] lines
         crates '()]
    (cond
      (= c \1)
      [(rest lines) (->> crates
                         (map #(filter (fn [v] v) %))
                         (map reverse)
                         (into []))]

      :else
      (recur lines (parse-line crates line)))))

(defn- parse-action [line]
  (let [[_ qty from to] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    (list (read-string qty) (dec (read-string from)) (dec (read-string to)))))

(defn- action [transform crates line]
  (let [[qty from to] (parse-action line)
        lfrom (get crates from)
        lto (concat (transform (take qty lfrom)) (get crates to))
        lfrom (drop qty lfrom)]
    (-> crates
        (assoc from lfrom)
        (assoc to lto))))

(defn- part [file transform]
  (with-open [rdr (reader file)]
    (let [[lines crates] (parse-crates (line-seq rdr))
          crates (reduce (partial action transform) crates lines)
          firsts (map first crates)]
      (apply str firsts))))

(defn part-1 [file]
  (part file reverse))

(defn part-2 [file]
  (part file identity))

#_(part-1 (clojure.java.io/resource "day05-example2"))
#_(part-1 (clojure.java.io/resource "day05-example"))
#_(part-1 (clojure.java.io/resource "day05"))
#_(part-2 (clojure.java.io/resource "day05-example"))
#_(part-2 (clojure.java.io/resource "day05"))
