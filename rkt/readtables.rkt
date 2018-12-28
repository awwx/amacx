#lang racket

(require "blockstr.rkt")
(require "curly.rkt")
(require "square.rkt")

(provide readtables w/readtables)

(define readtables
  (curly-readtable
    (square-readtable #f)))

(define (w/readtables port f)
  (with-blockstr-readtable readtables port f))


; (module+ test (require rackunit/chk)
;   (define (parse s)
;     (let ((in (open-input-string s)))
;       (w/readtables (λ () (read in)))))
;
;   (chk (parse "[a b c]")  '(square-bracket a b c))
;
;   (chk (parse "{ a, b 2 }") '(curly-bracket (a) (b 2))))
