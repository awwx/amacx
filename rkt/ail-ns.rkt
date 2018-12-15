#lang racket

(require racket/runtime-path)

(provide ail-ns
         default-mpair-namespace
         default-srcloc-namespace)

(module ail-mpair racket
  (provide quote-xVrP8JItk2Ot
           fn-xVrP8JItk2Ot
           assign-xVrP8JItk2Ot
           if-xVrP8JItk2Ot
           call-xVrP8JItk2Ot
           ns-var-xVrP8JItk2Ot
           #%top)
  (require (submod "runtime.rkt" mpair)))

(module ail-srcloc racket
  (provide quote-xVrP8JItk2Ot
           fn-xVrP8JItk2Ot
           assign-xVrP8JItk2Ot
           if-xVrP8JItk2Ot
           call-xVrP8JItk2Ot
           ns-var-xVrP8JItk2Ot
           #%top)
  (require (submod "runtime.rkt" srcloc)))

(define-namespace-anchor anchor)

(define-runtime-path me "ail-ns.rkt")

(define (ail-ns runtime)
  (define ns (namespace-anchor->empty-namespace anchor))
  (parameterize ((current-namespace ns))
    (namespace-require
      (case runtime
        ((mpair)  (list 'submod me 'ail-mpair))
        ((srcloc) (list 'submod me 'ail-srcloc))
        (else (error "invalid runtime" runtime)))))
  ns)

(define default-mpair-namespace  (ail-ns 'mpair))
(define default-srcloc-namespace (ail-ns 'srcloc))
