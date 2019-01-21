#lang racket

(require racket/hash)

(require "ail-ns.rkt")
(require "common.rkt")
(require "builtins.rkt")
(require "readtables.rkt")
(require "../arc/runtime.rkt")
(require "symtab.rkt")

(provide phase1 make-container populate-builtins new-container munch)

(define xboot-path (build-path rootdir "xboot"))

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

        ((caris x '$topvar--xVrP8JItk2Ot)
         `($topvar--xVrP8JItk2Ot
            ,(inject runtime container (second x))
            ,(third x)))

        ((pair? x)
         (cons (demunch runtime container (car x))
               (demunch runtime container (cdr x))))

        (else x)))

(define (exec1 runtime container x)
  (let ((a (demunch runtime container x)))
    (eval a (if (namespace? container)
                 container
                 (hash-ref default-namespaces runtime))))
  (void))

(define (file-each path f)
  (with-input-from-file path
    (λ ()
      (let loop ()
        (let ((x (read)))
          (unless (eof-object? x)
            (f x)
            (loop)))))))

(define (boot-file options)
  (build-path xboot-path
    (string-append
      "boot"
      (if (hash-ref options 'inline-tests #f) ".test" "")
      "."
      (symbol->string (hash-ref options 'topvar))
      "-topvar.nail")))

(define (make-container options)
  (case (hash-ref options 'topvar)
    ((ail) (construct-ail-ns (hash-ref options 'runtime)))
    ((ref) (new-symtab))
    (else
     (error "unknown topvar option" (hash-ref options 'topvar)))))

(define (populate-builtins options container)
  (define runtime (hash-ref options 'runtime))
  (define sref (runtimef runtime 'sref))
  (hash-for-each (runtime-builtins runtime)
    (λ (k v)
      (sref container k v)))
  (void))

(define (boot-container options)
  (define runtime (hash-ref options 'runtime))
  (define container (make-container options))
  (populate-builtins options container)
  container)

(define (phase1 options)
  (define runtime (hash-ref options 'runtime))
  (let ((container (boot-container options)))
    (file-each (boot-file options)
               (λ (x)
                 (exec1 runtime container x)))
    container))

(define (new-container options (container1 (phase1 options)))
  (define runtime (hash-ref options 'runtime))
  (((runtimef runtime 'ref) container1 'provision-container)
   (make-container options)
   (hash 'builtins (runtime-builtins runtime)
         'topvar   (hash-ref options 'topvar)
         'compiler ((runtimef runtime 'ref)
                     container1
                     'compile--xVrP8JItk2Ot)
         'validate-ail ((runtimef runtime 'tnil)
                        (hash-ref options 'validate-ail #f)))))

(define (munch runtime xs)
  (define options (hash 'runtime runtime 'topvar 'ail))
  (define container1 (phase1 options))
  (define aeval ((runtimef runtime 'ref) container1 'eval))
  (define container (new-container options container1))
  (for ((x (syntax->list xs)))
    (aeval x container)))
