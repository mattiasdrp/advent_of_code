(declare-fun z0 () Int)
(declare-fun z1 () Int)
(declare-fun w1 () Int)
(declare-fun z2 () Int)
(declare-fun w2 () Int)
(declare-fun z3 () Int)
(declare-fun w3 () Int)
(declare-fun z4 () Int)
(declare-fun w4 () Int)
(declare-fun z5 () Int)
(declare-fun w5 () Int)
(declare-fun z6 () Int)
(declare-fun w6 () Int)
(declare-fun z7 () Int)
(declare-fun w7 () Int)
(declare-fun z8 () Int)
(declare-fun w8 () Int)
(declare-fun z9 () Int)
(declare-fun w9 () Int)
(declare-fun z10 () Int)
(declare-fun w10 () Int)
(declare-fun z11 () Int)
(declare-fun w11 () Int)
(declare-fun z12 () Int)
(declare-fun w12 () Int)
(declare-fun z13 () Int)
(declare-fun w13 () Int)
(declare-fun z14 () Int)
(declare-fun w14 () Int)
; Constraints
(assert (= z0 0))
(assert (= z14 0))
(assert (>= w1 1))
(assert (<= w1 7))
(assert (>= w2 1))
(assert (<= w2 1))
(assert (>= w3 1))
(assert (<= w3 1))
(assert (>= w4 1))
(assert (<= w4 3))
(assert (>= w5 1))
(assert (<= w5 1))
(assert (>= w6 1))
(assert (<= w6 1))
(assert (>= w7 1))
(assert (<= w7 5))
(assert (>= w8 1))
(assert (<= w8 1))
(assert (>= w9 1))
(assert (<= w9 9))
(assert (>= w10 1))
(assert (<= w10 9))
(assert (>= w11 1))
(assert (<= w11 7))
(assert (>= w12 1))
(assert (<= w12 8))
(assert (>= w13 1))
(assert (<= w13 9))
(assert (>= w14 1))
(assert (<= w14 1))
(assert (= z1 (+ w1 0)))
(assert (= z2 (+ (* 26 z1) w2 12)))
(assert (= z3 (+ (* 26 z2) w3 14)))
(assert (= z4 (+ (* 26 z3) w4 0)))
(assert (= z5 (div z4 26)))
(assert (= w5 (- w4 2)))
(assert (= z6 (+ (* 26 z5) w6 15)))
(assert (= z7 (+ (* 26 z6) w7 11)))
(assert (= z8 (div z7 26)))
(assert (= w8 (- w7 4)))
(assert (= z9 (+ (* 26 z8) w9 1)))
(assert (= z10 (div z9 26)))
(assert (= w10 (- w9 8)))
(assert (= z11 (div z10 26)))
(assert (= w11 (- (mod z10 26) 9)))
(assert (= z12 (div z11 26)))
(assert (= w12 (- (mod z11 26) 7)))
(assert (= z13 (div z12 26)))
(assert (= w13 (- (mod z12 26) 4)))
(assert (= z14 (div z13 26)))
(assert (= w14 (- (mod z13 26) 6)))
(check-sat)
(get-model)
