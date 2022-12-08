(ns aoc-2022.day08
  (:require
   [aoc-utils.core :as utils]))

(defn- line->cells [line]
  (vec (map (fn [h] {:height (read-string (str h))}) line)))

(defn- append-cells [acc line]
  (let [cells (line->cells line)]
    (conj acc cells)))

(defn- update-cell [vcells row col maxh]
  (let [cell (get (get vcells row) col)
        height (:height cell)]
    [(assoc-in vcells [row col :visible] (or (:visible cell) (> height maxh)))
     (max height maxh)]))

(defn- set-visible [cells]
  (let [height (dec (count cells))
        width (dec (count (get cells 0)))]
    (loop [col 0
           row 0
           mh_left_col -1
           mh_right_col -1
           mh_down_row -1
           mh_up_row -1
           vcells cells]
      (if (> row height)
        vcells
        (if (> col width)
          (recur 0 (inc row) -1 -1 -1 -1 vcells)
          (let [[vcells mh_left_col] (update-cell vcells row col mh_left_col)
                [vcells mh_right_col] (update-cell vcells (- height row) (- width col) mh_right_col)
                [vcells mh_down_row] (update-cell vcells col row mh_down_row)
                [vcells mh_up_row] (update-cell vcells (- height col) (- width row) mh_up_row)]
            (recur (inc col) row
                   mh_left_col mh_right_col
                   mh_down_row mh_up_row
                   vcells)))))))

(defn part-1 [file]
  (let [cells (utils/reduce-file append-cells [] file)
        vcells (set-visible cells)
        count (reduce
               (fn [acc l]
                 (reduce
                  (fn [acc c] (if (:visible c) (inc acc) acc))
                  acc l))
               0 vcells)]
    count))

#_(part-1 (clojure.java.io/resource "day08"))

(defn- m-abs [n] (max n (- n)))

(defn- find-closer [index indexacc [_ nindex]]
  (if (> (m-abs (- index indexacc)) (m-abs (- index nindex))) nindex indexacc))

#_(into {} (filter #(> (first %) 3) {2 3, 3 4, 4 5}))

#_(reduce find-max 0 {2 7 3 4 4 5})

#_(assoc {2 7 3 4 4 5} 1 3)

(defn- update-cell-scenic [vcells row col index scenics]
  (let [cell (get (get vcells row) col)
        height (:height cell)
        scenics (into {} (filter #(>= (first %) height) scenics))
        nscenics (assoc scenics height index)
        closer (reduce (partial find-closer index) ##Inf scenics)
        nscenic (m-abs (- index closer))]
      ;; the closer pair is the one that blocks our view
      ;; (because its height is necessarily greater than the current one)
      ;; By construction we know that all the computed pairs indices are either
      ;; lower than the current one (left to right or up to down)
      ;; higher than the current one (right to left or down to up)
    [(assoc-in vcells [row col :scenic] (if-let [scenic (:scenic cell)] (* scenic nscenic) nscenic))
     nscenics]))

(defn- set-scenic [cells]
  (let [height (dec (count cells))
        width (dec (count (get cells 0)))
        base_lou {9 0}
        base_rod {9 height}]

    (loop [col 0
           row 0
           scenics_left_col base_lou
           scenics_right_col base_rod
           scenics_down_row base_lou
           scenics_up_row base_rod
           vcells cells]
      (if (> row height)
        vcells
        (if (> col width)
          (recur 0 (inc row) base_lou base_rod base_lou base_rod vcells)
          (let [[vcells scenics_left_col]
                (update-cell-scenic vcells row col col scenics_left_col)

                [vcells scenics_right_col]
                (update-cell-scenic vcells (- height row) (- width col) (- width col) scenics_right_col)

                [vcells scenics_down_row]
                (update-cell-scenic vcells col row col scenics_down_row)

                [vcells scenics_up_row]
                (update-cell-scenic vcells (- height col) (- width row) (- height col) scenics_up_row)]
            (recur (inc col) row
                   scenics_left_col scenics_right_col
                   scenics_down_row scenics_up_row
                   vcells)))))))

(defn part-2 [file]
  (let [cells (utils/reduce-file append-cells [] file)
        scells (set-scenic cells)
        count (reduce
               (fn [acc l]
                 (reduce (fn [acc c] (max acc (:scenic c))) acc l))
               0 scells)]
    count))

#_(part-2 (clojure.java.io/resource "day08-test"))

#_(part-2 (clojure.java.io/resource "day08"))
