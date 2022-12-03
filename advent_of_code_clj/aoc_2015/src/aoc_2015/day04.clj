(ns aoc-2015.day04
  (:require [clojure.string :as str])
  (:import
   java.security.MessageDigest
   java.math.BigInteger))

(defn- md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032X" (BigInteger. 1 raw))))

(defn part-1 [file]
  (let [k (str/trim (slurp file))
        res (for [i (iterate inc 1)
                  :let [hash (md5 (str k i))]
                  :when (= (subs hash 0 5) "00000")] i)]
    (first res)))

(defn part-2 [file]
  (let [k (str/trim (slurp file))
        res (for [i (iterate inc 1)
                  :let [hash (md5 (str k i))]
                  :when (= (subs hash 0 6) "000000")] i)]
    (first res)))

#_(part-1 (clojure.java.io/resource "day04"))
#_(part-2 (clojure.java.io/resource "day04"))
#_(md5 "abcdef609043")
#_(md5 "abcdef609043")
