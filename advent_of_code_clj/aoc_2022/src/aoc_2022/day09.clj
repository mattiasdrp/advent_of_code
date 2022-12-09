(ns aoc-2022.day09
  (:require
   [aoc-utils.core :as utils]
   [clojure.math :as math]))

(defn signum [x] (long (math/signum x)))

(defn update-coord [h t]
  (let [dx (- (:x h) (:x t))
        dy (- (:y h) (:y t))]
    (if (or (= (abs dx) 2) (= (abs dy) 2))
      (-> (update t :x #(+ % (signum dx)))
          (update :y #(+ % (signum dy))))
      t)))

(defn- move-h [h dir]
  (case dir
    :R (update h :x inc)
    :L (update h :x dec)
    :U (update h :y dec)
    :D (update h :y inc)))

(defn- move-list [h tail tset]
  (loop [head h
         [hd & tl] tail
         ntail '()]
    (cond
      (nil? hd)
      (let [pos9 (first ntail)
            tset (conj tset pos9)
            ntail (reverse ntail)]
        [ntail tset])

      :else
      (let [nhd (update-coord head hd)]
        (recur nhd tl (conj ntail nhd))))))

(defn- play-line [[h tail tset] line]
  (let [[_ dir cpt] (re-find #"(\w+) (\d+)" line)
        dir (keyword dir)
        cpt (read-string cpt)]
    (loop [i cpt
           h h
           tail tail
           tset tset]
      (if (= 0 i)
        [h tail tset]
        (let [h (move-h h dir)
              [tail tset] (move-list h tail tset)]
          (recur (dec i) h tail tset))))))

(defn part-1 [file]
  (let [start  {:x 0 :y 0}
        [_ _ tset] (utils/reduce-file play-line [start (list start) #{start}] file)]
    (count tset)))

(defn part-2 [file]
  (let [start {:x 0 :y 0}
        [_ _ tset] (utils/reduce-file play-line [start (repeat 9 start) #{start}] file)]
    (count tset)))

#_(part-1 (clojure.java.io/resource "day09"))
#_(part-2 (clojure.java.io/resource "day09"))
