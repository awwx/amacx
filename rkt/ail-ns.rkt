#lang racket

(require racket/runtime-path)

(require "common.rkt")
(require "../arc/runtime.rkt")

(provide construct-ail-ns default-namespaces)

(define-syntax-rule (create-ail-module name runtime)
  (module name racket
    (provide $quote--xVrP8JItk2Ot
             $fn--xVrP8JItk2Ot
             $assign--xVrP8JItk2Ot
             $if--xVrP8JItk2Ot
             $call--xVrP8JItk2Ot
             $ns-var--xVrP8JItk2Ot
             #%top)
    (require (submod "../arc/runtime.rkt" runtime))))

(create-ail-module ail-mpair  mpair)
(create-ail-module ail-srcloc srcloc)

(define-namespace-anchor anchor)

(define-runtime-path me "ail-ns.rkt")

(define (prefix-ail runtime)
  (string->symbol (string-append "ail-" (symbol->string runtime))))

(define (construct-ail-ns runtime)
  (define ns (namespace-anchor->empty-namespace anchor))
  (parameterize ((current-namespace ns))
    (namespace-require (list 'submod me (prefix-ail runtime))))
  ns)

(define default-namespaces
  (map-hash construct-ail-ns runtimes))
