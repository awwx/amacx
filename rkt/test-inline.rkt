#lang racket

(require "builtins.rkt")
(require "boot.rkt")
(require "readtables.rkt")
(require "runtime.rkt")
(require "symtab.rkt")

(print-hash-table #f)

(define (test-inline runtime)
  (define container1 (phase1 runtime #t))

  (printf "------ ~a phase two inline tests~n" runtime)

  (define container2
    (((runtimef runtime 'ref) container1 'provision-container)
     (new-symtab)
     (hash 'builtins       (runtime-builtins runtime)
           'macro-expander ((runtimef runtime 'ref) container1 'macro-expand)
           'inline-tests   ((runtimef runtime 'tnil) #t)
           'start          'container)))

  (printf "phase two ~a tests done\n" runtime)
  (symtab-rm container2 '*inline-tests*)

  container2)

(void
  (w/readtables
    (λ ()
      (for ((runtime runtimes))
        (test-inline runtime)))))
