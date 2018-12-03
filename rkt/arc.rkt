#lang racket

(require "boot.rkt")
(require "data.rkt")
(require "readtables.rkt")

(provide runarc)

(define (runarc)
  (define args (current-command-line-arguments))

  (w/readtables
    (λ ()
      (when (> (vector-length args) 0)
        (define file (vector-ref args 0))
        (define module (phase2 #f))
        (aload file module)
        (void)))))
