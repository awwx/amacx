#lang racket

(require racket/runtime-path)
(require "boot.rkt")

(define-runtime-path tests "../tests")

(define (runtest src)
  (printf "~s~n" src)
  (let ((module (phase2 #f)))
    (aload src module)))

(define (run-tests srcs)
  (for ((src srcs))
    (runtest src)))

(define (run-all-tests)
  (run-tests (map path->string (directory-list tests #:build? #t))))

(define argv (current-command-line-arguments))

(if (eqv? 0 (vector-length argv))
    (run-all-tests)
    (run-tests (vector->list argv)))
