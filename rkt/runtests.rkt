#lang racket

(require racket/runtime-path)
(require "boot.rkt")
(require "builtins.rkt")
(require "common.rkt")
(require "prefix.rkt")
(require "../arc/runtime.rkt")

(provide main)

(define-runtime-path here "here")

(define root (simplify-path (build-path here 'up 'up)))

(define (runtest options container1 src)
  (define runtime (hash-ref options 'runtime))
  (w/prefix (format "~a ~a " runtime src)
    (λ ()
      (printf "~a~n" src)
      (let ((container (new-container options container1)))
        ((runtimef runtime 'aload)
          (path->string (build-path root src))
          container
          container1)))))

(define (run-tests options container1 srcs)
  (for ((src srcs))
    (runtest options container1 src)))

(define (run-tests-in-runtime runtime srcs)
  (define options (hash 'runtime runtime
                        'topvar 'ail))
  (define container1 (phase1 options))
  (run-tests options container1 srcs))

(define (run-all-tests-in-runtime runtime)
  (run-tests-in-runtime runtime (all-tests)))

(define (run-all-tests)
  (for ((runtime runtimes))
    (run-all-tests-in-runtime runtime)))

(define (main . argv)
  (with-handlers (((λ (c)
                     (and (exn:fail? c)
                          (equal? (exn-message c)
                                  "error: test-failed")))
                   (λ (c)
                     (exit 1))))
    (cond ((null? argv)
           (run-all-tests))
          ((null? (cdr argv))
           (run-all-tests-in-runtime (string->symbol (car argv))))
          (else
           (run-tests-in-runtime (string->symbol (car argv))
                                 (cdr argv)))))
  (void))

(apply main (vector->list (current-command-line-arguments)))
