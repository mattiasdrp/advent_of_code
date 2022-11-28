(ns aoc-2020.day02
  (:require
   [aoc-utils.core :as utils]))

(defn- valid-passport1
  "Splits the `line` in low interval, high interval, the character `ch` to which the rule is applied and the passport that needs to be checked.
  Since maps are functions, we group all the same characters together in a map and get the seq corresponding to the `ch` and check that its cardinal is within bounds"
  [line]
  (let [[_ low high [ch] pass] (re-find #"(\d+)-(\d+)\s(\w):\s(\w+)" line)
        num (count ((group-by identity (seq pass)) ch))]
    (<= (read-string low) num (read-string high))))

#_(valid-passport1 "1-3 a: abcdea")
#_(not (valid-passport1 "1-3 a: abcdeaaa"))

(defn part-1 [file]
  (utils/reduce-file file
                     (fn [acc line] (if (valid-passport1 line) (inc acc) acc)) 0))

(defn- valid-passport2
  "Splits the `line` in first pos, second pos, the character `ch` to which the rule is applied and the passport that needs to be checked.
  Since maps are functions, we group all the same characters together in a map and get the seq corresponding to the `ch` and check that its cardinal is within bounds"
  [line]
  (let [[_ pos1 pos2 [ch] pass] (re-find #"(\d+)-(\d+)\s(\w):\s(\w+)" line)
        chars (vec pass)
        chpos1 (= ch (chars (dec (read-string pos1))))
        chpos2 (= ch (chars (dec (read-string pos2))))]
    (and (or chpos1 chpos2) (not (and chpos1 chpos2)))))

;; #_((re-find #"(\d+)-(\d+)\s(\w):\s(\w+)" "1-3 a: abcde")
#_(valid-passport2 "1-3 a: abcdea")
#_(not (valid-passport2 "1-3 a: abadeaaa"))

(defn part-2 [file]
  (utils/reduce-file file
                     (fn [acc line] (if (valid-passport2 line) (inc acc) acc)) 0))
