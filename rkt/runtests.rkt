#lang racket

(require racket/runtime-path)
(require "boot.rkt")

(define srcdirs '("src" "arcsrc" "tests"))

(define-runtime-path here "here")

(define root (simplify-path (build-path here 'up 'up)))

(define (runtest module1 src)
  (printf "~a~n" src)
  (let ((module (phase2 #f module1)))
    (aload (build-path root src) module)))

(define (run-tests srcs)
  (let ((module1 (phase1)))
    (for ((src srcs))
      (runtest module1 src))))

(define (tests-in-dir dir)
  (map (λ (filename)
         (path->string (build-path dir filename)))
       (filter (λ (filename)
                 (string-suffix? (path->string filename) ".t"))
               (directory-list (build-path root dir)))))

(define (all-tests)
  (append-map tests-in-dir srcdirs))

(define (run-all-tests)
  (run-tests (all-tests)))

(define argv (current-command-line-arguments))

(if (eqv? 0 (vector-length argv))
    (run-all-tests)
    (run-tests (vector->list argv)))
