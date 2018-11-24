#lang racket

(require racket/runtime-path)
(require "ail-ns.rkt")

(provide eval-ail)

(define ail-namespace (ail-ns))

(define (eval-ail code)
  (eval code ail-namespace))

(module+ test (require rackunit/chk)
  (require "symtab.rkt")

  (chk (eval-ail '(quote-xVrP8JItk2Ot 123)) 123

       (eval-ail '(call-xVrP8JItk2Ot
                    (fn-xVrP8JItk2Ot (fn)
                      (call-xVrP8JItk2Ot fn))
                    (fn-xVrP8JItk2Ot ()
                      (quote-xVrP8JItk2Ot 42))))
       42

       (eval-ail `(call-xVrP8JItk2Ot
                    (quote-xVrP8JItk2Ot ,(new-symtab (hash 'a 1)))
                    (quote-xVrP8JItk2Ot a)))
       1))
