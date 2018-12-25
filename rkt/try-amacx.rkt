#lang reader "amacx.rkt"

(use arcbase defloc)

(def one ()
  (+ 1 (/ 8 0)))

(def two ()
  (+ 2 (one)))

(def three ()
  (+ 3 (two)))

(three)
