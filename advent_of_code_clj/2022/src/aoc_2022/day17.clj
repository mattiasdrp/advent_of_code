(ns aoc-2022.day17
  (:require
   [clojure.string :as str]))

(def width 7)

(defn- translate [c shape chamber]
  (let [up (case c \< dec \> inc)
        nshape (update shape :shape #(map (fn [[y x]] [y (up x)]) %))]
    (if (first (filter (fn [[_ x :as coords]]
                         (or (= x 0) (= x 8) (chamber coords))) (:shape nshape)))
      shape nshape)))

(defn- down [shape chamber]
  (let [nshape (update shape :shape #(map (fn [[y x]] [(dec y) x]) %))]
    (if (first (filter (fn [coords] (chamber coords)) (:shape nshape)))
      (let [chamber (into chamber (:shape shape))
            [[yh _] & _] (:shape shape)]
        [shape chamber yh])
      [nshape nil nil])))

(defn- move [shape dir chamber]
  (-> (translate dir shape chamber)
      (down chamber)))

(defn- adjust-shape [shape height]
  (update shape :shape #(mapv (fn [[y x]] [(+ y height) (+ x 3)]) %)))

#_(into #{[1 2]} (:shape {:shape '([2 0] [2 2] [0 2] [1 2] [2 1]) :height 3, :id 3}))
#_(translate \< {:shape [[2 0] [2 2] [0 2] [1 2] [2 1]] :height 3, :id 3})
#_(translate \> {:shape [[2 0] [2 2] [0 2] [1 2] [2 1]] :height 3, :id 3})
#_(down (down (down (down {:shape [[0 0] [1 0] [2 0] [3 0]], :height 1}))))
#_(translate \> {:shape [[0 0] [1 0] [2 0] [3 0]], :height 1})
#_((into #{} (for [x (range 7)] [0 x])) [1 1])

(defn- next-shape [the_shapes [shape & rest] height]
  (cond
    (nil? shape)
    (recur nil the_shapes height)

    :else
    [(adjust-shape shape (+ height 4)) rest]))

(defn- pp-chamber [chamber shape height]
  (println shape)
  (doseq [y (range (+ 6 height) 0 -1)]
    (doseq [x (range 0 9)]
      (if (< 0 x 8)
        (if (chamber [y x])
          (print "#")
          (if ((into #{} (:shape shape)) [y x])
            (print "@")
            (print ".")))
        (print "|")))
    (println ""))
  (println ""))

(defn- main-loop [the_shapes the_dirs limit slice_size]
  (let [[curr_shape shapes] (next-shape nil the_shapes 0)]
    (loop [cpt 0
           height 0
           chamber (into #{} (for [x (range 7)] [0 x]))
           shapes shapes
           curr_shape curr_shape
           [dir & rest_dirs] the_dirs
           pattern_memo {}]
      (cond
        (= cpt limit)
        height

        (nil? dir)
        (recur cpt height chamber shapes curr_shape the_dirs pattern_memo)

        :else
        (let [[shape nchamber yh] (move curr_shape dir chamber)]
          (cond
            ;; If no chamber has been returned, the shape hasn't stopped moving
            (nil? nchamber)
            (recur cpt height chamber shapes shape rest_dirs pattern_memo)

            :else
            (let [height (max height yh)
                  [nshape shapes] (next-shape the_shapes shapes height)
                  [pattern_memo & rest]
                  (if (and (some? pattern_memo) (= 1 (:id shape)))
                    ;; When resetting the shape ring we store:
                    ;; - the current dir counter
                    ;; - a slice of the chamber (from height to ?)
                    ;; If such a configuration was already stored, we reached a pattern
                    (let [min_height (max 0 (- height slice_size))
                          dir_count (count rest_dirs)
                          slice_chamber (filter (fn [[y _]] (<= min_height y)) chamber)
                          slice_chamber (map (fn [[y x]] [(- y min_height) x]) slice_chamber)
                          slice_chamber (into #{} slice_chamber)
                          [height1 cpt1 _] (first
                                            (filter
                                             (fn [[_ _ schamber]]
                                               (= schamber slice_chamber))
                                             (pattern_memo dir_count)))]
                      (cond
                        (nil? height1)
                        [(update pattern_memo dir_count conj [height cpt slice_chamber])]

                        :else
                        [nil height1 cpt1]))
                    [pattern_memo])]
              (cond
                (nil? rest)
                  ;; (pp-chamber chamber curr_shape height)
                (recur (inc cpt) height nchamber shapes nshape rest_dirs pattern_memo)

                :else

                (let [[height1 cpt1] rest
                      diff_cpt (- cpt cpt1)
                      diff_height (- height height1)
                      loops (quot limit diff_cpt)
                      loops (if (rem limit diff_cpt) (- loops 2) loops)
                      ncpt (+ cpt1 (* loops diff_cpt))
                      nheight (+ height1 (* diff_height loops))
                      raise (- nheight height)
                      chamber (into #{} (map (fn [[y x]] [(+ y raise) x]) nchamber))
                      shape (update nshape :shape #(mapv (fn [[y x]] [(+ y raise) x]) %))]

                  (recur (inc ncpt) nheight chamber shapes shape rest_dirs nil))))))))))

(defn- part [file slice_size limit]
  (let [line (str/trim (slurp file))
        the_dirs (into (vector) line)
        ;; Shapes are of topmost & rest
        horiz {:shape [[0 0] [0 3] [0 1] [0 2]], :id 1}
        cross {:shape [[2 1] [1 0] [1 2] [0 1] [1 1]]}
        angle {:shape [[2 2] [0 0] [0 2]  [0 1] [1 2]]}
        vert {:shape [[3 0] [1 0] [2 0]  [0 0]]}
        square {:shape [[1 0] [0 0] [0 1]  [1 1]]}
        the_shapes (vector horiz cross angle vert square)
        height (main-loop the_shapes the_dirs limit slice_size)]
    height))

(defn part-1 [file slice_size] (part file slice_size 2022))
(defn part-2 [file slice_size] (part file slice_size 1000000000000))

#_(part-1 (clojure.java.io/resource "day17-example" 10))

#_(count ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
