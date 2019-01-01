#lang racket

(require racket/hash)

(require "ail-ns.rkt")
(require "common.rkt")
(require "builtins.rkt")
(require "readtables.rkt")
(require "../arc/runtime.rkt")
(require "symtab.rkt")

(provide phase1 new-container munch)

(define expanded-boot-path (build-path rootdir "xboot/compiler.nail"))
(define test-boot-path     (build-path rootdir "xboot/compiler-test.nail"))

;; Phase one

(define (inject runtime container x)
  (cond ((box? x)
         (cond ((eq? (unbox x) 'this-container)
                container)
               (else
                ((runtimef runtime 'ref)
                  container
                  (unbox x)))))

        ((pair? x)
         (cons (inject runtime container (car x))
               (inject runtime container (cdr x))))

        (else
         x)))

(define (caris x v)
  (and (pair? x) (eq? (car x) v)))

(define (demunch runtime container x)
  (cond ((caris x '$quote--xVrP8JItk2Ot)
         `($quote--xVrP8JItk2Ot
            ,((runtimef runtime 'quote-this)
               (inject runtime container (cadr x)))))

        ((pair? x)
         (cons (demunch runtime container (car x))
               (demunch runtime container (cdr x))))

        (else x)))

(define (exec1 runtime container x)
  (let ((a (demunch runtime container x)))
    (eval a (hash-ref default-namespaces runtime)))
  (void))

(define (file-each path f)
  (with-input-from-file path
    (λ ()
      (let loop ()
        (let ((x (read)))
          (unless (eof-object? x)
            (f x)
            (loop)))))))

(define (phase1 runtime (inline-tests #f))
  (let ((container (new-symtab (runtime-builtins runtime))))
    (file-each (if inline-tests test-boot-path expanded-boot-path)
               (λ (x)
                 (exec1 runtime container x)))
    container))

(define (new-container runtime (container1 (phase1 runtime)))
  (((runtimef runtime 'ref) container1 'provision-container)
   (new-symtab)
   (hash 'builtins (runtime-builtins runtime)
         'compiler ((runtimef runtime 'ref)
                     container1
                     'compile--xVrP8JItk2Ot))))

(define (munch runtime xs)
  (define container1 (phase1 runtime #f))
  (define aeval ((runtimef runtime 'ref) container1 'eval))
  (define container (new-container runtime container1))
  (for ((x (syntax->list xs)))
    (aeval x container)))
