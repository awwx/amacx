#lang racket

(require "boot.rkt")
(require "readtables.rkt")

(print-hash-table #f)

(void
  (w/readtables
    (λ ()
      (phase2 'mpair  #t)
      (phase2 'srcloc #t))))
