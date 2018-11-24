#lang racket

(require racket/runtime-path)
(require "data.rkt")
(require "symtab.rkt")

(define-runtime-path ail-path "ail.rkt")

(provide eval-ail)

(define ail-namespace (make-base-empty-namespace))

(define-runtime-module-path data-path "data.rkt")
(define-namespace-anchor here)
(define host (namespace-anchor->namespace here))
(parameterize ((current-namespace ail-namespace))
  (namespace-attach-module host data-path)
  (namespace-require ail-path))

(define (eval-ail code)
  (parameterize ((current-namespace ail-namespace))
    (eval code)))

(module+ test (require rackunit/chk)
  (chk (eval-ail '(quote-xVrP8JItk2Ot 123)) 123

       (eval-ail '(call-xVrP8JItk2Ot (fn-xVrP8JItk2Ot (fn) (call-xVrP8JItk2Ot fn))
                                        (fn-xVrP8JItk2Ot () (quote-xVrP8JItk2Ot 42))))
       42

       (eval-ail `(call-xVrP8JItk2Ot (quote-xVrP8JItk2Ot ,(new-symtab (hash 'a 1))) (quote-xVrP8JItk2Ot a)))
       1))
