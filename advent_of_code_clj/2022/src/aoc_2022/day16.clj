(ns aoc-2022.day16
  (:require
   [clojure.pprint]
   [aoc-utils.core :as utils]
   [clojure.string :as str]))

(defn- parse-line [[max_rate bitvalue mmap] line]
  (println "parsing" line)
  (let [[_ name rate valves] (re-find #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([\w, ]+)" line)
        name (keyword name)
        rate (parse-long rate)
        valves (into #{} (map keyword (str/split valves #", ")))]
    [(max max_rate rate) (inc bitvalue) (assoc mmap name {:rate rate :bitvalue bitvalue :children valves})]))

(defn- graph-search [graph max_rate]
  (let [max_minute 30
        best_total (atom 0)
        aux_fn
        (fn aux_fn [node minute backward opened total curr_flow]
          ;; (println node "min:" minute "open:" opened "tot:" total "flow:" curr_flow)
          (cond
            (>= minute max_minute)
            (when (= minute max_minute)
              (let [total (+ total curr_flow)]
                (swap! best_total #(max % total))))

            (let [rem_minute (- (inc max_minute) minute)
                  hyp_total (+ total
                               (* rem_minute curr_flow)
                               (/ (* rem_minute rem_minute max_rate) 3))]
              (< hyp_total @best_total))
            nil

            :else
            (let [content (graph node)
                  total (+ total curr_flow)]
            ;; jump to children without opening the valve
              (when-not
               (or (= (:rate content) 0)
                   (bit-test opened (:bitvalue content)))
                (aux_fn node (inc minute)
                        0
                        (bit-set opened (:bitvalue content))
                        total
                        (+ curr_flow (:rate content))))

              (doseq [child (:children content)]
                (let [content (graph child)]
                  (when-not (bit-test backward (:bitvalue content))
                    (aux_fn child (inc minute) (bit-set backward (:bitvalue content))
                            opened total curr_flow)))))))]

    (aux_fn :AA 1 0 0 0 0)
    @best_total))

(defn part-1 [file]
  (let [[max_rate _ graph] (utils/reduce-file parse-line [0 1 {}] file)]
    (println graph)
    (println max_rate)
    (graph-search graph max_rate)))

#_(part-1 (clojure.java.io/resource "day16-example"))

(defn- graph-search-elephant [graph max_rate]
  (let [max_minute 26
        best_total (atom 0)
        aux-fn
        (fn aux-fn [prev_me node_me prev_el node_el minute opened total curr_flow]
          (cond

            ;; Time out
            (>= minute max_minute)
            (when (= minute max_minute)
              (let [total (+ total curr_flow)]
                (swap! best_total #(max % total))))

            ;; This branch will never reach best_total
            (let [rem_minute (- (inc max_minute) minute)
                  hyp_total (+ total
                               (* rem_minute curr_flow)
                               (/ (* rem_minute rem_minute max_rate) 3))]
              (< hyp_total @best_total))
            nil

            :else
            (let [content_me (graph node_me)
                  content_el (graph node_el)
                  minute (inc minute)
                  total (+ total curr_flow)]

              (when-not
               (or
                (= node_me node_el)
                (= (:rate content_el) 0)
                (= (:rate content_me) 0)
                (bit-test opened (:bitvalue content_me))
                (bit-test opened (:bitvalue content_el)))
                (aux-fn
                 nil node_me
                 nil node_el
                 minute
                 (-> (bit-set opened (:bitvalue content_me))
                     (bit-set (:bitvalue content_el)))
                 total (+ curr_flow (:rate content_el) (:rate content_me))))

              (when-not
               (or
                (= (:rate content_el) 0)
                (bit-test opened (:bitvalue content_el)))
                (doseq [child_me (disj (:children content_me) prev_me)]
                  (aux-fn
                   nil child_me
                   nil node_el
                   minute
                   (bit-set opened (:bitvalue content_el))
                   total (+ curr_flow (:rate content_el)))))

              (when-not
               (or
                (= node_me node_el)
                (= (:rate content_me) 0)
                (bit-test opened (:bitvalue content_me)))
                (doseq [child_el (disj (:children content_el) prev_el)]
                  (aux-fn
                   nil node_me
                   nil child_el
                   minute
                   (bit-set opened (:bitvalue content_me))
                   total (+ curr_flow (:rate content_me)))))

              (let [comb_close_close
                    (for [child_me (disj (:children content_me) prev_el)
                          child_el (disj  (:children content_el) prev_me)]
                      [child_me child_el])
                    comb_close_close (into #{} comb_close_close)]
                (doseq [[child_me child_el] comb_close_close]
                  (aux-fn
                   node_me child_me
                   node_el child_el
                   minute opened
                   total curr_flow))))))]
    (aux-fn nil :AA nil :AA 1 0 0 0)
    @best_total))

(defn part-2 [file]
  (let [[max_rate _ graph] (utils/reduce-file parse-line [0 1 {}] file)
        res (graph-search-elephant graph max_rate)]
    (println res)
    res))

#_(part-2 (clojure.java.io/resource "day16-example"))
