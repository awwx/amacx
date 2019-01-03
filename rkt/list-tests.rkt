#lang racket

(require json)
(require "../arc/runtime.rkt")
(require "common.rkt")

(define (firstn n xs)
  (cond ((null? xs)
         xs)
        ((and (> n 0) (not (null? xs)))
         (cons (car xs) (firstn (- n 1) (cdr xs))))
        (else '())))

(define (nthcdr n xs)
  (cond ((null? xs)
         '())
        ((> n 0)
         (nthcdr (- n 1) (cdr xs)))
        (else xs)))

(define (tuples xs n)
  (if (null? xs)
      '()
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

(write-json
  (append-map (λ (runtime)
                (map (λ (testlist)
                       (cons (symbol->string runtime) testlist))
                     (tuples (all-tests) 2)))
              runtimes))
(newline)
