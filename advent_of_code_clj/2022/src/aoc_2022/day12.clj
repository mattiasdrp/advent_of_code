(ns aoc-2022.day12
  (:require
   [clojure.java.io :refer [reader]]))

(defn- coord->key [x y] {:x x :y y})

(defn- line->vertices [graph [row line]]
  (let [iseq (map-indexed vector line)]
    (reduce (fn [graph [col char]]
              (case char
                \S (->
                    (assoc graph :start (coord->key row col))
                    (assoc (coord->key row col) 0))
                \E (->
                    (assoc graph :end (coord->key row col))
                    (assoc (coord->key row col) 25))
                (assoc graph (coord->key row col)  (- (int char) (int \a)))))
            graph iseq)))

(defn- neighbours [graph k v pred]
  (let [{:keys [x y]} k
        add-if-in-bounds
        (fn [coll x y]
          (if-let [v' (get graph (coord->key x y))]
            (if (pred v v')
              (conj coll (coord->key x y))
              coll)
            coll))]
    (-> #{}
        (add-if-in-bounds (dec x) y)
        (add-if-in-bounds (inc x) y)
        (add-if-in-bounds x (dec y))
        (add-if-in-bounds x (inc y)))))

(defn update-map [m f]
  (reduce-kv (fn [m k v]
               (assoc m k {:children (f k v)})) {} m))

(defn- fill-children [graph pred]
  (update-map graph (fn [k v] (neighbours graph k v pred))))

(defn- bfs
  [graph start pred]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{}
         prev {}]
    (cond
      (pred (peek queue))
      (loop [curr (peek queue)
             path '()]
        (cond
          (nil? (prev curr))
          (count path)

          :else
          (let [curr (prev curr)
                path (conj path curr)]
            (recur curr path))))

      (visited (peek queue))
      (recur (pop queue) visited prev)

      (empty? queue)
      ##Inf

      :else
      (let [curr (peek queue)
            visited (conj visited curr)
            nodes (:children (graph curr))
            nodes (remove visited nodes)
            queue (into (pop queue) nodes)
            prev (reduce (fn [prev node] (assoc prev node curr)) prev nodes)]
        (recur queue visited prev)))))

(defn part-1 [file]
  (let [graph (with-open [rdr (reader file)]
                (let [lines (map-indexed vector (line-seq rdr))]
                  (reduce line->vertices {} lines)))
        start (:start graph)
        end (:end graph)
        graph (dissoc (dissoc graph :start) :end)
        ngraph (fill-children graph (fn [v v'] (<= v' (inc v))))
        res (bfs ngraph start (fn [node] (= node end)))]
    res))

(defn part-2 [file]
  (let [graph (with-open [rdr (reader file)]
                (let [lines (map-indexed vector (line-seq rdr))]
                  (reduce line->vertices {} lines)))
        end (:end graph)
        graph (dissoc (dissoc graph :start) :end)
        ngraph (fill-children graph (fn [v v'] (<= v (inc v'))))
        res (bfs ngraph end (fn [node] (= (get graph node) 0)))]
    res))

#_(part-1 (clojure.java.io/resource "day12-example"))
#_(line->vertices {} "SbcdeE" 0)
#_(map-indexed vector (str/split "abcz" #""))
