(ns aoc-2022.day23
  (:require
   [aoc-utils.core :as utils]))

(defn- parse-line [[mset row] line]
  (let [cset (map-indexed (fn [idx c] [idx c]) line)]
    (loop [[[idx c] & rest] cset
           mset mset]
      (cond
        (nil? idx)
        mset

        (= c \.)
        (recur rest mset)

        :else
        (recur rest (conj mset [row idx]))))))

(defn- check-neighbours [mset propositions directions [row col :as c]]
  (let [neighbours (mapv (fn [[dr dc]] [(+ row dr) (+ col dc)]) directions)
        occupied (mapv (fn [c] [c (contains? mset c)]) neighbours)
        empty (first (filter (fn [[_ t]] t) occupied))]
    (cond
      (nil? empty)
      propositions

      :else
      (loop [[[c1 o1] [c2 o2] [c3 o3] & rest] occupied]
        (cond
          ;; No possible move
          (nil? c1)
          propositions

          ;; Empty 3 cells
          (and (not o1) (not o2) (not o3))
          (cond
            (get propositions c2)
            (dissoc propositions c2)

            :else
            (assoc propositions c2 c))

          :else
          (recur rest))))))

(defn- apply-propositions [propositions mset]
  (reduce-kv (fn [mset new old] (-> (disj mset old)
                                    (conj new)))
             mset propositions))

(defn- count-empty [mset]
  (let [[minr maxr minc maxc elves]
        (reduce
         (fn [[minr maxr minc maxc elves] [row col]]
           [(min row minr)
            (max row maxr)
            (min col minc)
            (max col maxc)
            (inc elves)]) [0 0 0 0 0] mset)
        width (- (inc maxc) minc)
        height (- (inc maxr) minr)
        area (* width height)
        res (- area elves)]
    res))

(defn- pp [mset]
  (let [[minr maxr minc maxc]
        (reduce
         (fn [[minr maxr minc maxc] [row col]]
           [(min row minr) (max row maxr) (min col minc) (max col maxc)]) [0 0 0 0] mset)]
    (doseq [row (range minr (inc maxr))]
      (doseq [col (range minc (inc maxc))]
        (if (mset [row col])
          (print "#")
          (print ".")))
      (println ""))))

(defn- mloop [mset rounds-limit]
  (let [directions
        [;; north
         [-1 -1] [-1 0] [-1 +1]
         ;; south
         [+1 -1] [+1 0] [+1 +1]
         ;; west
         [-1 -1] [0 -1] [+1 -1]
         ;; east
         [-1 +1] [0 +1] [+1 +1]]]
    (loop [[x y z & rest :as directions] directions
           mset mset
           rounds 1]
      (cond
        (> rounds rounds-limit)
        mset

        :else
        (let [propositions (reduce (fn [propositions c] (check-neighbours mset propositions directions c)) {} mset)
              mset (apply-propositions propositions mset)]
          ;; (println "directions" directions "\nproposition")
          ;; (clojure.pprint/pprint propositions)
          ;; (println "mset")
          ;; (clojure.pprint/pprint mset)
          ;; (pp mset)
          (cond
            (empty? propositions)
            rounds

            :else
            (recur (conj (vec rest) x y z) mset (inc rounds))))))))

#_(check-neighbours #{[4 8]} {} directions [4 9])

(defn part-1 [file]
  (let [[mset _] (utils/reduce-file (fn [[_ row :as acc] line] [(parse-line acc line) (inc row)]) [#{} 0] file)
        mset (mloop mset 10)]
    (count-empty mset)))

(defn part-2 [file]
  (let [[mset _] (utils/reduce-file (fn [[_ row :as acc] line] [(parse-line acc line) (inc row)]) [#{} 0] file)
        res (mloop mset ##Inf)]
    res))

#_(part-1 (clojure.java.io/resource "day23-example"))
