(ns aoc-2022.day21
  (:require [aoc-utils.core :as utils]))

(defn- parse-line [acc line]
  (let [[_ id val] (re-find #"(\w+): (\d+)" line)]
    (cond
      (nil? val)
      (let [[_ id id1 op id2] (re-find #"(\w+): (\w+) ([-+*/]) (\w+)" line)]
        (assoc acc id {:operation [id1  op id2]}))

      :else
      (assoc acc id {:val (parse-long val)}))))

(defn- exact-eval [mmap node]
  (let [value (get mmap node)
        val (:val value)]
    (cond
      (nil? val)
      (let [[idl op idr] (:operation value)
            vall (exact-eval mmap idl)
            valr (exact-eval mmap idr)
            val ((resolve (symbol (read-string op))) vall valr)]
        val)
      :else
      val)))

(defn part-1 [file]
  (let [mmap (utils/reduce-file parse-line {} file)
        res (exact-eval mmap "root")]
    res))

#_(part-1 (clojure.java.io/resource "day21-example"))

(defn- eval-conj [mmap node]
  (let [value (get mmap node)
        val (:val value)]
    (cond
      (= "humn" node)
      (list true "humn")

      (nil? val)
      (let [[idl op idr] (:operation value)
            [humnl _ :as left] (eval-conj mmap idl)
            [humnr _ :as right] (eval-conj mmap idr)
            val {:operation (list  op left right)}]
        (list (or humnl humnr) val))
      :else
      (list false {:val val}))))

(defn- list-eval [l]
  (let [val (:val l)]
    (cond
      (nil? val)
      (let [[op [_ left] [_ right]] (:operation l)
            left (list-eval left)
            right (list-eval right)]
        ((resolve (symbol (read-string op))) left right))

      :else
      val)))

(defn- invop [op]
  (case op
    "+" -
    "-" +
    "*" /
    "/" *
    (assert false)))

(defn- partial-eval [l const]
  (let [val (:val l)]
    (cond
      (= "humn" l)
      const

      (nil? val)
      (let [[op [humnl left] [_ right]] (:operation l)]
        ;; the form is (op (...humn...) v) or (op v (...humn...) v)
        ;; If op is + or *, the order doesn't matter
        ;; -  the result is const = invop const v
        ;; Otherwise
        ;; - In the first case, const = (invop const v) (if c = x/v then x = c * v)
        ;; - In the second case, const = (op v const) (if c = v/x then x = v / c)
        (cond
          (or (= op "+") (= op "*"))
          (let [[v humn] (if humnl
                           [(list-eval right) left]
                           [(list-eval left) right])
                const ((invop op) const v)]
            (partial-eval humn const))

          humnl
          (let [const ((invop op) const (list-eval right))]
            (partial-eval left const))

          :else
          (let [const ((resolve (symbol (read-string op))) (list-eval left) const)]
            (partial-eval right const))))
      :else
      val)))

(defn part-2 [file]
  (let [mmap (utils/reduce-file parse-line {} file)
        [left _ right] (:operation (get mmap "root"))
        [humnl resl] (eval-conj mmap left)
        [humnr resr] (eval-conj mmap right)
        const (if humnl (list-eval resr) (list-eval resl))
        partial (if humnl (partial-eval resl const) (partial-eval resr const))]
    partial))

#_(part-2 (clojure.java.io/resource "day22-example2"))
