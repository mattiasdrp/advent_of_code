(ns aoc-2020.day04
  (:require [aoc-utils.core :as utils]))

;; -----------
;; Part 1
;; -----------

(comment
  ;; First version, creating the whole passport and checking its cardinality
  ;; The first first version created a hash map but I wanted to experiment with sets even if it means duplicating some code for the second part

  (defn- parse-partial-passport [pass line]
    (into pass (flatten (map rest (re-seq #"(\w+):[^\s]+" line)))))

  (defn- valid?
    "Since there are no duplicated fields we can just check that the number of fields that are not \"cid\" is 7"
    [passport]
    (= (count (disj passport "cid")) 7))

  (defn- parse-or-return
    [{:keys [res curr_pass] :as acc} line]
    (cond
      (empty? line) {:res (if (valid? curr_pass) (inc res) res) :curr_pass #{}}
      :else (update acc :curr_pass parse-partial-passport line)))

  (defn part-1 [file]
    (:res (utils/reduce-file-newline file parse-or-return {:res 0 :curr_pass #{}}))))

;; Second version, start from the set of all possible fields and remove them one by one

(def fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn- parse-partial-passport [fields line]
  (apply disj fields (flatten (map rest (re-seq #"(\w+):[^\s]+" line)))))

(defn- parse-or-return
  [{:keys [res rem_fields] :as acc} line]
  (cond
    (empty? line) {:res (if (empty? rem_fields) (inc res) res) :rem_fields fields}
    :else (update acc :rem_fields parse-partial-passport line)))

(defn part-1 [file]
  (:res (utils/reduce-file-newline file parse-or-return {:res 0 :rem_fields fields})))

;; -----------
;; Part 2
;; -----------

(defn- valid-int? [value low high]
  (and (re-matches #"\d+" (or value ""))
       (<= low (read-string value) high)))

(defn- valid-height? [height]
  (let [[_ val unit] (re-find #"(\d+)(\w+)" (or height ""))]
    (cond
      (= unit "cm") (<= 150 (read-string val) 193)
      (= unit "in") (<= 59 (read-string val) 76)
      :else false)))

(defn- valid-hair? [hair]
  (re-matches #"#[a-f0-9]{6}" (or hair "")))

(defn- valid-eyes? [eyes]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} eyes))

(defn- valid-pid? [pid]
  (re-matches #"^\d{9}$" (or pid "")))

(defn- valid-passport? [passport]
  (and
   (valid-int? (passport "byr") 1920 2002)
   (valid-int? (passport "iyr") 2010 2020)
   (valid-int? (passport "eyr") 2020 2030)
   (valid-height? (passport "hgt"))
   (valid-hair? (passport "hcl"))
   (valid-eyes? (passport "ecl"))
   (valid-pid? (passport "pid"))))

(defn- parse-partial-passport2 [pass line]
  (into pass (apply hash-map (flatten (map rest (re-seq #"(\w+):([^\s]+)" line))))))

(defn- parse-or-return2
  [{:keys [res curr_pass] :as acc} line]
  (cond
    (empty? line) {:res (if (valid-passport? curr_pass) (inc res) res) :curr_pass {}}
    :else (update acc :curr_pass parse-partial-passport2 line)))

(defn part-2 [file]
  (:res (utils/reduce-file-newline file parse-or-return2 {:res 0 :curr_pass {}})))

#_(part-2 (clojure.java.io/resource "day04-example"))
#_(valid-passport? (parse-partial-passport2 {} "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"))
#_(valid-eyes? ((parse-partial-passport2 {} "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f") "ecl"))
