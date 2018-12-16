#lang racket

(require "common.rkt")
(require "symtab.rkt")

(provide (all-defined-out))

(define-syntax-rule (unwrap x)
  x)

(define-syntax-rule (unwrap-list xs)
  xs)

(define-syntax-rule (unwrap-args f)
  f)

(define-syntax-rule (deep-unwrap x)
  x)

(define-syntax unwraps
  (syntax-rules ()
    ((unwraps (v ...) body ...)
     (begin body ...))))

; TODO just nil
(define (ar-false? x)
  (or (eq? x 'nil) (eq? x #f)))

(define (ar-true? x)
  (not (ar-false? x)))

(define xcons mcons)

(define acons mpair?)

(define (xcar x)
  (cond ((mpair? x)
         (mcar x))
        ((eq? x 'nil)
         'nil)
        (else
         ; TODO err
         (error "Can't take car of" x))))

(define (xcdr x)
  (cond ((mpair? x)
         (mcdr x))
        ((eq? x 'nil)
         'nil)
        (else
         ; TODO err
         (error "Can't take cdr of" x))))

(define (ar-is a b)
  (or (eqv? a b)
      (and (string? a) (string? b) (string=? a b))
      (and (ar-false? a) (ar-false? b))))

(define (ar-srcloc loc x)
  x)

(define-syntax-rule (hide v)
  v)

(define-syntax-rule (unhide x)
  x)

(define (ar-denil x)
  (cond ((mpair? x)
         (cons (ar-denil-car (mcar x))
               (ar-denil-cdr (mcdr x))))
        (else x)))

(define (ar-denil-car x)
  (if (eq? x 'nil)
      'nil
      (ar-denil x)))

(define (ar-denil-cdr x)
  (if (eq? x 'nil)
      '()
      (ar-denil x)))

(define (builtin-loc x)
  'nil)

(define (hasloc x)
  #f)
