#lang racket

(require racket/runtime-path)
(require "boot.rkt")
(require "builtins.rkt")
(require "readtables.rkt")
(require "runtime.rkt")

(provide main)

(define-runtime-path here "here")

(define root (simplify-path (build-path here 'up 'up)))

(define (runtest runtime module1 src)
  (printf "~a ~a~n" runtime src)
  (let ((module (new-container runtime module1)))
    ((runtimef runtime 'aload)
      (path->string (build-path root src))
      module
      module1)))

(define (run-tests runtime module1 srcs)
  (for ((src srcs))
    (runtest runtime module1 src)))

(define (run-tests-in-runtime runtime srcs)
  (let ((module1 (phase1 runtime)))
    (run-tests runtime module1 srcs)))

(define (run-all-tests-in-runtime runtime)
  (run-tests-in-runtime runtime (all-tests)))

(define (run-all-tests)
  (for ((runtime runtimes))
    (run-all-tests-in-runtime runtime)))

(define (main . argv)
  (w/readtables
    (λ ()
      (cond ((null? argv)
             (run-all-tests))
            ((null? (cdr argv))
             (run-all-tests-in-runtime (string->symbol (car argv))))
            (else
             (run-tests-in-runtime (string->symbol (car argv))
                                   (cdr argv))))))
  (void))
