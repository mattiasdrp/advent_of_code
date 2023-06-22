(ns aoc-2022.day11
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn- parse-op [line]
  (let [[_ op e2] (re-find #"new = [\w\d]+ ([-+*/]) ([\w\d]+)" line)
        op (resolve (symbol (read-string op)))
        e2 (read-string e2)]
    (if (int? e2)
      (fn [old] (op old e2))
      (fn [old] (op old old)))))

(defn- bored-op1 [val _] (math/floor-div val 3))
(defn- bored-op2 [val m] (mod val m))

(defn- parse-monkey [[mmap tests] lines]
  (let [[mk it op te tru fal] (str/split lines #"\n")
        monkey (parse-long (re-find #"\d+" mk))
        items (vec (map parse-long (re-seq #"\d+" it)))
        op (parse-op op)
        test (parse-long (re-find #"\d+" te))
        tests (conj tests test)
        tru (parse-long (re-find #"\d+" tru))
        fal (parse-long (re-find #"\d+" fal))
        update (fn [mmap newv]
                 (update-in mmap
                            [(if (= 0 (mod newv test)) tru fal) :items]
                            conj newv))]
    [(assoc mmap monkey {:items items :op op :update update :visited 0})
     tests]))

(defn- part [file loops bored-op]
  (let [content (slurp file)
        lines (str/split content #"\n\n")
        [monkeys tests] (reduce parse-monkey (list {} '()) lines)
        modulo (reduce * tests)]
    (loop [cpt 0
           monkey 0
           monkeys monkeys]
      (if (= cpt loops)
        (->> (sort-by (fn [[_ v]] (- (:visited v))) monkeys)
             (take 2)
             (reduce (fn [acc [_ v]] (* acc (:visited v))) 1))

        (if-let [{:keys [items op update]} (get monkeys monkey)]

          (let [monkeys (update-in monkeys [monkey :visited]
                                   (partial + (count (get-in monkeys [monkey :items]))))
                monkeys (assoc-in monkeys [monkey :items] [])
                monkeys (reduce (fn [monkeys old]
                                  (let [newv (bored-op (op old) modulo)]
                                    (update monkeys newv))) monkeys items)]
            (recur cpt (inc monkey) monkeys))

          (recur (inc cpt) 0 monkeys))))))

(defn part-1 [file] (part file 20 bored-op1))
(defn part-2 [file] (part file 10000 bored-op2))

#_(part-1 (slurp (clojure.java.io/resource "day11")))
#_(part-2 (slurp (clojure.java.io/resource "day11")))
