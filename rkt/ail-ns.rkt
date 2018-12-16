#lang racket

(require racket/runtime-path)

(require "common.rkt")
(require "runtime.rkt")

(provide ail-ns default-namespaces)

(define-syntax-rule (create-ail-module name runtime)
  (module name racket
    (provide quote-xVrP8JItk2Ot
             fn-xVrP8JItk2Ot
             assign-xVrP8JItk2Ot
             if-xVrP8JItk2Ot
             call-xVrP8JItk2Ot
             ns-var-xVrP8JItk2Ot
             #%top)
    (require (submod "runtime.rkt" runtime))))

(create-ail-module ail-mpair  mpair)
(create-ail-module ail-srcloc srcloc)

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

(define default-namespaces
  (map-hash ail-ns runtimes))
