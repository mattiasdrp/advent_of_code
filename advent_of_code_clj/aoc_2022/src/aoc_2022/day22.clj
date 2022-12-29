(ns aoc-2022.day22
  (:require
   [aoc-utils.core :as utils]))

;; A line is either
;;       ...###...
;; ...###...
;;
;; If the line starts with a blank space, the last char is not one
;; If the line starts with a non blank space, the last char isn't one either
(defn- parse-line [[row mmap transposed] line]
  (let [[_ blanks text] (re-find #"(\s*)([#.]+)" line)
        blanks (count blanks)
        left-bound blanks
        right-bound (+ blanks (dec (count text)))
        cseq (map-indexed (fn [idx c] [(+ left-bound idx) c]) text)]
    (loop [[[idx c] & tl] cseq
           left right-bound
           mmap mmap]
      (cond
        (nil? tl)
        (if transposed
          (update mmap [row idx] merge {[0 -1] [row left] [0 1] [row left-bound]})
          (assoc mmap [idx row] {:type c [-1 0] [left row] [1 0] [left-bound row]}))

        :else
        (let [mmap
              (if transposed
                (update mmap [row idx] merge {[0 -1] [row left] [0 1] [row (inc idx)]})
                (assoc mmap [idx row] {:type c [-1 0] [left row] [1 0] [(inc idx) row]}))]
          (recur tl idx mmap))))))

(defn- find-start [matrix]
  (let [line (first matrix)
        [_ blanks] (re-find #"(\s*)[#.]+" line)]
    {:coords [(count blanks) 0] :direction [1 0]}))

(defn- transpose [matrix]
  (apply map str matrix))

(defn- normalize [matrix length]
  (map (fn [s] (str s (apply str (take (- length (count s)) (repeat \space))))) matrix))

(defn- rotate [curr r]
  (case r
    "R" (update curr :direction
                (fn [[col row]]
                  (if (= 0 col)
                    [(- row) col]
                    [row col])))
    "L" (update curr :direction
                (fn [[col row]]
                  (if (= 0 col)
                    [row col]
                    [row (- col)])))))

(defn- move [mmap curr times]
  (let [coords (:coords curr)
        dir (:direction curr)]
    (loop [coords coords
           times times]
      (cond
        (= 0 times)
        (assoc curr :coords coords)

        :else
        (let [next ((mmap coords) dir)]
          (if (= (:type (mmap next)) \#)
            (assoc curr :coords coords)
            (recur next (dec times))))))))

(defn- travel [mmap start path]
  (loop [curr start
         [hd & tl] path]
    (cond
      (nil? hd)
      curr

      :else
      (let [order (parse-long hd)]
        (cond
          (nil? order)
          (recur (rotate curr hd) tl)

          :else
          (recur (move mmap curr order) tl))))))

(defn- facing [[col row]]
  (cond
    (= 1 col)
    0
    (= 1 row)
    1
    (= -1 col)
    2
    :else
    3))

(defn- coords->int [[col row]]
  (+ (* 1000 (inc row)) (* 4 (inc col))))

(defn part-1 [file]
  (let [[matrix mmap length path]
        (utils/reduce-file
         (fn [[matrix row mmap length last] line]
           (if last
             [matrix mmap length line]
             (if (empty? line)
               [matrix row mmap length true]
               (let [mmap (parse-line [row mmap false] line)
                     matrix (conj matrix line)]
                 [matrix (inc row) mmap (max length (count line)) false]))))
         [[] 0 {} 0 false] file)

        [_ mmap]
        (reduce
         (fn [[col mmap] line]
           [(inc col) (parse-line [col mmap true] line)])
         [0 mmap] (transpose (normalize matrix length)))

        start (find-start matrix)
        path (re-seq #"\d+|\w" path)
        {:keys [coords direction]}  (travel mmap start path)
        score (+ (coords->int coords) (facing direction))]
    score))

#_(part-1 (clojure.java.io/resource "day22-example2"))

;; Cube of size C rules
;; If row = 0, when going [0 -1]
;;   - col is between 2*C and 2*(C+1)
;;   - we're going to row = C
;;     -
