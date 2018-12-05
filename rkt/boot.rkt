#lang racket

(require racket/hash)
(require racket/runtime-path)

(require "ail-ns.rkt")
(require "aload.rkt")
(require "builtins.rkt")
(require "data.rkt")
(require "symtab.rkt")

(provide phase1 phase2 aload)

(define expanded-boot-path (build-path rootdir "xcompile/boot.expanded"))

;; Phase one

(define (inject module x)
  (cond ((box? x)
         (cond ((eq? (unbox x) '*module*)
                module)
               (else
                (ref module (unbox x)))))

        ((pair? x)
         (cons (inject module (car x))
               (inject module (cdr x))))

        (else
         x)))

(define (caris x v)
  (and (pair? x) (eq? (car x) v)))

(define (demunch module x)
  (cond ((caris x 'quote-xVrP8JItk2Ot)
         `(quote-xVrP8JItk2Ot ,(ar-niltree (inject module (cadr x)))))

        ((pair? x)
         (cons (demunch module (car x))
               (demunch module (cdr x))))

        (else x)))

(define ail-namespace1 (ail-ns))

(define (exec1 module x)
  (let ((a (demunch module x)))
    (eval a ail-namespace1))
  (void))

(define (file-each path f)
  (with-input-from-file path
    (λ ()
      (let loop ()
        (let ((x (read)))
          (unless (eof-object? x)
            (f x)
            (loop)))))))

(define (phase1 (inline-tests #f))
  (when inline-tests
    (printf "------ phase one~n"))
  (let ((module (new-symtab builtins)))
    (file-each expanded-boot-path (λ (x) (exec1 module x)))
    module))


;; Phase two

(define (phase2 inline-tests (module1 (phase1 inline-tests)))
  (when inline-tests
    (printf "------ phase two~n"))

  (define module2
    ((ref module1 'provision-container)
     (new-symtab)
     (hash 'builtins       builtins
           'macro-expander (ref module1 'macro-expand)
           'inline-tests   (tnil inline-tests)
           'start          'use-implementation)))

  (when inline-tests
    (printf "phase two tests done\n")
    (symtab-rm module2 '*inline-tests*))

  module2)
