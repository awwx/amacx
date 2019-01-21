#lang racket

(require "ail-ns.rkt")
(require "builtins.rkt")
(require "boot.rkt")
(require "prefix.rkt")
(require "readtables.rkt")
(require "../arc/runtime.rkt")

(define (test-inline options)
  (define runtime (hash-ref options 'runtime))

  (define container1
    (w/prefix (format "~a ~a: phase one: " runtime (hash-ref options 'topvar))
      (λ ()
        (phase1 options))))

  (newline)

  (define container2 (make-container options))
  (populate-builtins options container2)

  (w/prefix (format "~a ~a: phase two: " runtime (hash-ref options 'topvar))
    (λ ()
      (((runtimef runtime 'ref) container1 'provision-container)
       container2
       (hash 'builtins      (runtime-builtins runtime)
             'topvar        (hash-ref options 'topvar)
             'compiler      ((runtimef runtime 'ref)
                              container1
                              'compile--xVrP8JItk2Ot)
             'inline-tests  ((runtimef runtime 'tnil) #t)
             'start         'container))))

  (void))

(void
  (for ((runtime runtimes))
    (for ((topvar '(ail ref)))
      (let ((options (hash 'runtime runtime
                           'topvar topvar
                           'inline-tests #t)))
        (test-inline options)))))
