(ns aoc-utils.core
  (:require    [clojure.java.io :refer [reader]]))

(defn do-seq-file
  "Takes a `file` and a function `f` and applies `f` to each line of `file`
  This function returns `nil` since the result of `f line` is not stored anywhere"

  [file f]
  (with-open [rdr (reader file)]
    (doseq [input (line-seq rdr)] (f input))))

(defn reduce-file
  "Takes a `file`, a function `f` and, optionally, an `acc` applies `f` to `acc` and `l1` or `l1` and `l2` then to the result of this application and the next line etc. until the end and returns the last computed result"

  ([f file]
   (with-open [rdr (reader file)]
     (reduce f (line-seq rdr))))

  ([f acc file]
   (with-open [rdr (reader file)]
     (reduce f acc (line-seq rdr)))))

(defn reduce-file-newline
  "Same as `reduce-file` but will apply `f` in the end to an empty string"
  ([f file]
   (f (reduce-file f file) ""))

  ([f acc file]
   (f (reduce-file f acc file) "")))

(defn do-all-file
  "Takes a `file` and a function `f` and creates a sequence `(f l1, f l2, ..., f ln)` where `l1, l2, ..., ln` are the lines of `file`"

  [file f]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (map f (line-seq rdr)))))

(defn update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k (f v))) {} m))
