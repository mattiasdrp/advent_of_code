(ns aoc-2022.day07
  (:require [aoc-utils.core :as utils]
            [clojure.pprint]))

;; A map of dirs
;; a dir is {:parent string :children #{node set}}
;; a node is either a string related to a dir or a {:name string :size int}

(defn- add-child [filesystem dir child]
  (update filesystem dir #(update % :children conj child)))

(defn- custom-name [curr_node dir] (str curr_node dir))

(defn- move-and-update [[filesystem curr_node] line]
  ;; if "cd dir" change the current node
  (if-let [[_ dir] (re-find #"cd (\w+|/)" line)]
    (let [name (custom-name curr_node dir)]
      [(assoc filesystem name {:parent curr_node :children #{}}) name])
    ;; if "cd .." go to the parent of the current node
    (if (re-find #"cd .." line)
      [filesystem (:parent (get filesystem curr_node))]
        ;; if "int name", add this file (we don't care about the parent)
        ;; and add this file as its parent child
      (if-let [[_ size name] (re-find #"(\d+) ([\w.]+)" line)]
        (let [filesystem (add-child filesystem curr_node {:size (read-string size) :name name})]
          [filesystem curr_node])
        ;; if "dir name" add this dir with curr_node as its parent
        (if-let [[_ name] (re-find #"dir ([\w.]+)" line)]
          [(add-child filesystem curr_node (custom-name curr_node name)) curr_node]
          ;; if ls, nothing to do
          [filesystem curr_node])))))

(defn compute-size [filesystem]
  (letfn
   [(compute-size [filesystem node]
      ;; node is {:size int :name string}
      (if-let [size (:size node)]
        [filesystem size]
        ;; node is string (size is computed by traversing children)
        ;; Or size has already been computed and stored
        (let [nmap (get filesystem node)]
          (if-let [size (:size nmap)]
            [filesystem size]
            (let [children (:children nmap)
                  [filesystem size] (reduce update-children [filesystem 0] children)
                  filesystem (update filesystem node assoc :size size)]
              [filesystem size])))))

    (update-children [[filesystem acc] child]
      (let [[filesystem size] (compute-size filesystem child)]
        [filesystem (+ size acc)]))]

    (let [[fs _] (compute-size filesystem "/")]
      fs)))

(defn- find-below [filesystem n]
  (reduce-kv (fn [acc _ {:keys [size]}]
               (if (<= size n) (+ acc size) acc)) 0 filesystem))

(defn part-1 [file]
  (let [[fs _] (utils/reduce-file move-and-update [{} nil] file)]
    (find-below (compute-size fs) 100000)))

#_(part-1 (clojure.java.io/resource "day07"))

(defn part-2 [file]
  (let [[fs _] (utils/reduce-file move-and-update [{} nil] file)
        fs (compute-size fs)
        total-size (:size (get fs "/"))
        ;; total 70_000_000
        currently-unused (- 70000000 total-size)
        expected-unused 30000000]
    (reduce-kv (fn [dir_size _ {:keys [size]}]
                 (let [unused (+ currently-unused size)]
                   (if (and (>= unused expected-unused) (<= size dir_size))
                     size
                     dir_size))) total-size fs)))

#_(part-2 (clojure.java.io/resource "day07"))
