#lang racket

(require racket/runtime-path)

(require "ar.rkt")
(require "eval-ail.rkt")
(require "symtab.rkt")

(define-runtime-path here "here")

(define (from-here filename)
  (simplify-path (build-path here 'up filename)))

(define expanded-boot-path (from-here "../xcompile/boot.expanded"))


;; Phase one

(define (caris x v)
  (and (pair? x) (eq? (car x) v)))

(define module1 (builtins))

(define (inject x)
  (cond ((box? x)
         (cond ((eq? (unbox x) '*module*)
                module1)
               (else
                (hash-ref module1 (unbox x)))))

        ((pair? x)
         (cons (inject (car x))
               (inject (cdr x))))

        (else
         x)))

(define (demunch x)
  (cond ((caris x 'quote-xVrP8JItk2Ot)
         `(quote-xVrP8JItk2Ot ,(ar-niltree (inject (cadr x)))))

        ((pair? x)
         (cons (demunch (car x))
               (demunch (cdr x))))

        (else x)))

(define (mcaris x v)
  (and (mpair? x) (eq? (mcar x) v)))

(define (mcadr x)
  (mcar (mcdr x)))

; This is like ar-denil except that quoted values are left unconverted.

(define (arcail x)
  (cond ((mcaris x 'quote-xVrP8JItk2Ot)
         (list 'quote-xVrP8JItk2Ot (mcadr x)))

        ((mpair? x)
         (cons (arcail-car (mcar x))
               (arcail-cdr (mcdr x))))

        (else x)))

(define (arcail-car x)
  (if (eq? x 'nil)
      'nil
      (arcail x)))

(define (arcail-cdr x)
  (if (eq? x 'nil)
      '()
      (arcail x)))

(print-hash-table #f)

(define (exec1 x)
  (let ((a (demunch x)))
    (eval-ail a))
  (void))

(define (file-each path f)
  (with-input-from-file path
    (λ ()
      (let loop ()
        (let ((x (read)))
          (unless (eof-object? x)
            (f x)
            (loop)))))))

(file-each expanded-boot-path exec1)

(define (macro-expand1 module x)
  ((hash-ref module1 'macro-expand)
   (hash 'module module 'validate (λ (x) x))
   x))

(define (arc-eval1 module code)
  (define m
    (macro-expand1 module (ar-niltree code)))
  (define ail (arcail m))
  (eval-ail ail))


;; Phase two

(define include-tests #t)

(when include-tests
  (printf "------ phase two~n"))

(define (replace-tree mapping x)
  (cond ((and (symbol? x)
              (hash-has-key? mapping x))
         (hash-ref mapping x))
        ((pair? x)
         (cons (replace-tree mapping (car x))
               (replace-tree mapping (cdr x))))
        (else x)))

(define $renames
  (hash '$quote   'quote-xVrP8JItk2Ot
        '$fn      'fn-xVrP8JItk2Ot
        '$assign  'assign-xVrP8JItk2Ot
        '$if      'if-xVrP8JItk2Ot
        '$call    'call-xVrP8JItk2Ot))

(define (rename$ x)
  (replace-tree $renames x))

(define module2 (new-symtab (builtins)))

(define (exec2 x)
  (arc-eval1 module2 (rename$ x)))

(define (load-test filename)
  (file-each
    (from-here (string-append "../tests/" (symbol->string filename) ".t"))
    (λ (x)
      (exec2 x))))

(define (include2 x)
  (cond ((caris x 'test)
         (when include-tests
           (load-test (cadr x))))
        (else
         (exec2 x))))

(define (load2 filename)
  (file-each (from-here filename) include2))

(load2 "../src/boot.arc")
(load2 "../qq/qq.arc")
(load2 "../src/two.arc")

(define (macro-expand2 module x)
  ((symtab-ref module2 'macro-expand)
   (hash 'module module 'validate (λ (x) x))
   x))

(define (arc-eval2 module code)
  (eval-ail
    (arcail
      (macro-expand2 module (ar-niltree code)))))

(define (aload module filename)
  (file-each filename
    (λ (x)
      (arc-eval2 module x))))

(when include-tests
  (aload module2 "perftest.arc"))
