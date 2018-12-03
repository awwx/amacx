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

  (define module2 (new-symtab builtins))

  (when inline-tests
    (sref module2 '*inline-tests* 't))

  (sref module2 'use
    ((ref module1 'implement-use) module2))

  (sref module2 'provides
    ((ref module1 'implement-provides) module2))

  (aload 'macro module2 module1)
  (aload 'asfilename module2 module1)
  (aload 'findfile module2 module1)
  (aload 'implement-use module2 module1)
  (aload 'eval module2 module1)
  (aload 'file-each module2 module1)

  (when inline-tests
    (printf "phase two tests done\n")
    (symtab-rm module2 '*inline-tests*))

  module2)
