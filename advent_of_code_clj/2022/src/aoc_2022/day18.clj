(ns aoc-2022.day18
  (:require
   [aoc-utils.core :as utils]
   [clojure.set :as set]))

(defn- parse-line [[minx maxx miny maxy minz maxz] line]
  (let [coords (re-seq #"\d+" line)
        [x y z] (map parse-long coords)]
    [[(min x minx) (max x maxx) (min y miny) (max y maxy) (min z minz) (max z maxz)]
     (vector x y z)]))

(defn- add-cube [[coords cubes] line]
  (let [[coords cube] (parse-line coords line)]
    [coords (conj cubes cube)]))

(defn- neighbours [[x y z]]
  (let [cseq [-1 0 1]]
    ;; Generating a sequence of direct neigbbours
    ;; Those are the neighbours that are only one coordinate ± 1
    (for [dx cseq, dy cseq, dz cseq
          :when (= 1 (+ (abs dx) (abs dy) (abs dz)))]
      [(+ x dx) (+ y dy) (+ z dz)])))

;; The algorithm works this way:
;; - for each cube, generate all its neighbours
;; - remove from the generated neighbours all the cubes that are present
;; - count
(defn- untouched_neighbours [cubes]
  (->>
   cubes
   (mapcat neighbours)
   (remove cubes)))

(defn- mark-empty [[x y z :as empty] [minx maxx miny maxy minz maxz :as maxcoords]
                   cubes interiors exteriors unmarked]
  ;; (println "marking" empty)
  ;; Go right
  (cond
    ;; If the empty cube is outside of the limits or already marked
    ;; as an exterior then all the children we went through are exteriors
    (or (< x minx) (> x maxx) (< y miny) (> y maxy) (< z minz) (> z maxz)
        (exteriors empty))
    [1 (set/union exteriors unmarked)]

    ;; If the current empty cube is a cube or was an unmarked cube,
    ;; we already went through it and can't conclude
    (or (cubes empty) (unmarked empty))
    [0 unmarked]

    (interiors empty)
    [0 (set/union interiors unmarked)]

    :else
    ;; (let [nbrs (conj empty (neighbours empty))

    (let [unmarked (conj unmarked empty)
          [res-rightx children] (mark-empty [(dec x) y z] maxcoords cubes interiors exteriors unmarked)]
      ;; if res = 1 then children is exteriors updated
      (if (= 1 res-rightx)
        [1 children]
        ;; Else, children is unmarked updated, call recursively
        (let [empty [(inc x) y z]
              [res-leftx children] (mark-empty empty  maxcoords cubes interiors exteriors children)]
          (if (= 1 res-leftx)
            [1 children]
            (let [empty [x (dec y) z]
                  [res-righty children] (mark-empty empty maxcoords cubes interiors exteriors children)]
              (if (= 1 res-righty)
                [1 children]
                (let [empty [x (inc y) z]
                      [res-lefty children] (mark-empty empty maxcoords cubes interiors exteriors children)]
                  (if (= 1 res-lefty)
                    [1 children]
                    (let [empty [x y (dec z)]
                          [res-rightz children] (mark-empty empty maxcoords cubes interiors exteriors children)]
                      (if (= 1 res-rightz)
                        [1 children]
                        (let [empty [x y (inc z)]
                              [res-leftz children] (mark-empty empty maxcoords cubes interiors exteriors children)]
                          (if (= 1 res-leftz)
                            [1 children]
                            ;; We traversed all the children and never reached the exterior
                            ;; The children is all the unmarked empty cubes, mark them as interior
                            [0 (set/union interiors children)]))))))))))))))

(defn part-1 [file]
  (let [[coords cubes]
        (utils/reduce-file add-cube
                           [[##Inf ##-Inf ##Inf ##-Inf ##Inf ##-Inf] '()] file)
        cubes (into #{} cubes)]
    (count (untouched_neighbours cubes))))

(defn part-2 [file]
  (let [[[minx maxx miny maxy minz maxz] cubes]
        (utils/reduce-file add-cube
                           [[##Inf ##-Inf ##Inf ##-Inf ##Inf ##-Inf] '()] file)
        coords [(dec minx) (inc maxx) (dec miny) (inc maxy) (dec minz) (inc maxz)]
        cubes (into #{} cubes)
        emptys (untouched_neighbours cubes)
        [res interiors exteriors]
        (reduce (fn [[cpt interiors exteriors] empty]
                  (let [[res set] (mark-empty empty coords cubes interiors exteriors #{})]
                    ;; (println "exteriors" (into (sorted-set) exteriors))
                    ;; (println "interiors" (into (sorted-set) interiors))
                    (if (= res 1)
                      [(inc cpt) interiors set]
                      [cpt set exteriors]))) [0 #{} #{}] emptys)]
    ;; (println "exteriors" (into (sorted-set) exteriors))
    ;; (println "interiors" (into (sorted-set) interiors))
    ;; (println "untouched" (into (sorted-set) emptys))
    ;; (println "interiors2:" (set/difference (into #{} emptys) exteriors))
    [res (count exteriors)]))

#_(parse-line "1,2,3")
