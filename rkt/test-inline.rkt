#lang racket

(require "builtins.rkt")
(require "boot.rkt")
(require "prefix.rkt")
(require "readtables.rkt")
(require "runtime.rkt")
(require "symtab.rkt")

(print-hash-table #f)

(define (test-inline runtime)
  (define container1
    (w/prefix (format "~a phase one " runtime)
      (λ ()
        (phase1 runtime #t))))

  (newline)

  (define container2
    (w/prefix (format "~a phase two " runtime)
      (λ ()
        (((runtimef runtime 'ref) container1 'provision-container)
         (new-symtab)
         (hash 'builtins      (runtime-builtins runtime)
               'compiler      ((runtimef runtime 'ref)
                                container1
                                'compile-xVrP8JItk2Ot)
               'inline-tests  ((runtimef runtime 'tnil) #t)
               'start         'container)))))

  (symtab-rm container2 '*inline-tests*)
  (void))

(void
  (for ((runtime runtimes))
    (test-inline runtime)))
