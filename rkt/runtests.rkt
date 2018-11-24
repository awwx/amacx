#lang racket

(require racket/runtime-path)
(require "boot.rkt")

(define-runtime-path tests "../tests")

(define (runtest module1 src)
  (printf "~s~n" src)
  (let ((module (phase2 #f module1)))
    (aload src module)))

(define (run-tests srcs)
  (let ((module1 (phase1)))
    (for ((src srcs))
      (runtest module1 src))))

(define (run-all-tests)
  (run-tests (map path->string (directory-list tests #:build? #t))))

(define argv (current-command-line-arguments))

(if (eqv? 0 (vector-length argv))
    (run-all-tests)
    (run-tests (vector->list argv)))
