#lang racket

(require racket/hash)

(require "common.rkt")
(require "builtins.rkt")
(require "readtables.rkt")
(require "symtab.rkt")

(provide phase1 phase2 new-container munch)

(define expanded-boot-path (build-path rootdir "xboot/boot.expanded"))
(define test-boot-path     (build-path rootdir "xboot/boot-test.expanded"))

;; Phase one

(define (inject runtime container x)
  (cond ((box? x)
         (cond ((eq? (unbox x) '*module*)
                container)
               (else
                (((xruntime runtime) 'ref)
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
  (cond ((caris x 'quote-xVrP8JItk2Ot)
         `(quote-xVrP8JItk2Ot
            ,(((xruntime runtime) 'quote-this)
              (inject runtime container (cadr x)))))

        ((pair? x)
         (cons (demunch runtime container (car x))
               (demunch runtime container (cdr x))))

        (else x)))

(define (exec1 runtime container x)
  (let ((a (demunch runtime container x)))
    (eval a ((xruntime runtime) 'namespace)))
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
  (when inline-tests
    (printf "~a ------ phase one~n" runtime))
  (let ((container (new-symtab ((xruntime runtime) 'builtins))))
    (file-each (if inline-tests test-boot-path expanded-boot-path)
               (λ (x)
                 (exec1 runtime container x)))
    container))


;; Phase two

(define (phase2 runtime inline-tests (container1 (phase1 runtime inline-tests)))
  (when inline-tests
    (printf "~a ------ phase two~n" runtime))

  (define container2
    ((((xruntime runtime) 'ref) container1 'provision-container)
     (new-symtab)
     (hash 'builtins       ((xruntime runtime) 'builtins)
           'macro-expander (((xruntime runtime) 'ref) container1 'macro-expand)
           'inline-tests   (((xruntime runtime) 'tnil) inline-tests)
           'start          'container)))

  (when inline-tests
    (printf "phase two tests done\n")
    (symtab-rm container2 '*inline-tests*))

  container2)

(define (new-container runtime (container1 (phase1 runtime)))
  ((((xruntime runtime) 'ref) container1 'provision-container)
   (new-symtab)
   (hash 'builtins ((xruntime runtime) 'builtins)
         'macro-expander (((xruntime runtime) 'ref) container1 'macro-expand))))

(define (munch runtime xs)
  (define container1 (phase1 runtime #f))
  (define aeval (((xruntime runtime) 'ref) container1 'eval))
  (define container (new-container runtime container1))
  (w/readtables
    (λ ()
      (for ((x (syntax->list xs)))
        (aeval x container)))))
