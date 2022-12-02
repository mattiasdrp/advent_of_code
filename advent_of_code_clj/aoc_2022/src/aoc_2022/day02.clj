(ns aoc-2022.day02
  (:require [aoc-utils.core :as utils]))

;; We decide that A/X are 0, B/Y 1 and C/Z 2
;; If cp = (ce + 1 mod 3) then cp beats ce because:
;;  - 0 beats 2: 0 = 2 + 1 mod 3
;;  - 2 beats 1: 2 = 1 + 1
;;  - 1 beats 0: 1 = 0 + 1
(defn- shifumi [ce cp]
  (cond
    (= cp ce) 0
    (= cp (mod (inc ce) 3)) 1
    :else -1))

(defn- score [ce cp]
  (+ (inc cp) (+ 3 (* (shifumi ce cp) 3))))

(defn part-1 [file]
  (utils/reduce-file file (fn [acc line]
                            (let [[ce _ cp] (seq line)
                                  ce (- (int ce) (int \A))
                                  cp (- (int cp) (int \X))]
                              (+ acc (score ce cp)))) 0))

;; Once again, we decide to place ourselves in ℤ/3ℤ
;; We transform X, must lose, in -1, Y, draw, in 0 and Z, must win, in 1
;; If an elf chose 'c' and we need to beat it we need to take the number after it
;; which is perfect since Z is 1 so just (c + 1) mod 3
;; The same goes for lose, -1 mod 3 and draw, + 0
(defn- player-choice [ce cres]
  (inc (mod (+ ce cres) 3)))

(defn- score-choice [ce cres]
  (+ (player-choice ce cres) (+ 3 (* cres 3))))

(defn part-2 [file]
  (utils/reduce-file file (fn [acc line]
                            (let [[ce _ cres] (seq line)
                                  ce (- (int ce) (int \A))
                                  cres (- (int cres) (int \Y))]
                              (+ acc (score-choice ce cres)))) 0))
