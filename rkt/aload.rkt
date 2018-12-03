#lang racket

(require racket/hash)
(require racket/runtime-path)
(require "ail-ns.rkt")
(require "data.rkt")

(provide aload rootdir eval-ail)

(define-runtime-path here "here")

(define rootdir (path->string (simplify-path (build-path here 'up 'up))))

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

(define default-ail-namespace (ail-ns))

(define (eval-ail x (ns default-ail-namespace))
  (eval (arcail x) ns))

(define (aload name
               target-module
               (macro-module target-module))
  ((or (ref target-module 'load #f)
       (ref macro-module  'load))
   name
   target-module))
