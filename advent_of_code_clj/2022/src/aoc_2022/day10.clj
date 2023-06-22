(ns aoc-2022.day10
  (:require [aoc-utils.core :as utils]))

(defn- clock? [clock] (= (mod (- clock 20) 40) 0))

(defn- parse-line [[x clock acc] line]
  (if (> clock 220)
    (reduced acc)
    (if (= line "noop")
      (let [acc (if (clock? clock) (+ acc (* clock x)) acc)
            clock (inc clock)]
        [x clock acc])
      (let [val (parse-long (re-find #"-?\d+" line))
            acc (if-let
                 [clock (if (clock? clock)
                          clock
                          (if (clock? (inc clock))
                            (inc clock)
                            nil))]
                  (+ acc (* clock x))
                  acc)
            x (+ x val)
            clock (+ clock 2)]
        [x clock acc]))))

(defn part-1 [file]
  (utils/reduce-file parse-line [1 1 0 '()] file))

#_(part-1 (clojure.java.io/resource "day10"))

(defn- inc-crt [crt]
  (let [crt (inc crt)]
    (if (> crt 40)
      (do
        (println "")
        1)
      crt)))

(defn- print-if-visible [x crt]
  (print (if (<= (dec x) (dec crt) (inc x))
           "#" ".")))

(defn- parse-line-crt [[x crt] line]
  (if (> crt 240)
    (reduced nil)
    (if (= line "noop")
      (do (print-if-visible x crt)
          [x (inc-crt crt)])
      (let [val (parse-long (re-find #"-?\d+" line))]
        (print-if-visible x crt)
        (let [crt (inc-crt crt)]
          (print-if-visible x crt)
          [(+ x val) (inc-crt crt)])))))

(defn part-2 [file]
  (utils/reduce-file parse-line-crt [1 1] file))

#_(part-2 (clojure.java.io/resource "day10"))
