#lang racket

(require racket/runtime-path)
(require "data.rkt")

(provide ail-ns)

(define-runtime-path ail-path "ail.rkt")
(define-runtime-module-path data-path "data.rkt")
(define-namespace-anchor here)
(define host (namespace-anchor->namespace here))

(define (ail-ns)
  (define ail-namespace (make-base-empty-namespace))
  (parameterize ((current-namespace ail-namespace))
    (namespace-attach-module host data-path)
    (namespace-require ail-path))
  ail-namespace)
