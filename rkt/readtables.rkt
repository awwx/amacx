#lang racket

(require "square.rkt")

(provide w/readtables)

(define readtables
  (square-readtable #f))

(define (w/readtables f)
  (parameterize ((current-readtable readtables))
    (f)))

(module+ test (require rackunit/chk)
  (define (parse s)
    (let ((in (open-input-string s)))
      (w/readtables (λ () (read in)))))

  (chk (parse "[a b c]")  '(square-bracket a b c)))
