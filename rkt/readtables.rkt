#lang racket

(require "curly.rkt")
(require "square.rkt")

(provide w/readtables)

(define readtables
  (curly-readtable
    (square-readtable #f)))

(define (w/readtables f)
  (parameterize ((current-readtable readtables))
    (f)))

(module+ test (require rackunit/chk)
  (define (parse s)
    (let ((in (open-input-string s)))
      (w/readtables (λ () (read in)))))

  (chk (parse "[a b c]")  '(square-bracket a b c))

  (chk (parse "{ a, b 2 }") '(curly-bracket (a) (b 2))))
