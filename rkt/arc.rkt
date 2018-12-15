#lang racket

(require "boot.rkt")
(require "builtins.rkt")
(require "readtables.rkt")

(provide runarc)

(define (runarc runtime)
  (define args (current-command-line-arguments))

  (w/readtables
    (λ ()
      (when (> (vector-length args) 0)
        (define file (vector-ref args 0))
        (define module1 (phase1 runtime))
        (define module (new-container runtime module1))
        ((runtimef runtime 'sref)
         module
         'argv
         ((runtimef runtime 'ar-nillist) (cdr (vector->list args))))
        ((runtimef runtime 'aload) file module module1)
        (void)))))
