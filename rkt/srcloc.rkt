#lang racket

(require syntax/srcloc)

(require "common.rkt")
(require "symtab.rkt")

(provide (all-defined-out))

(define (unwrap x)
  (if (syntax? x)
      (unwrap (syntax-e x))
      x))

; TODO prob replace with deep-unwrap
(define (unwrap-list xs)
  (map unwrap xs))

(define (unwrap-args f)
  (λ args
    (apply f (map unwrap args))))

(define-syntax unwraps
  (syntax-rules ()
    ((unwraps (v ...) body ...)
     (let ((v (unwrap v)) ...)
       body ...))))

(define (ar-false? x)
  (let ((x (unwrap x)))
    (or (eq? x 'nil) (eq? x #f) (null? x))))

(define (ar-true? x)
  (not (ar-false? x)))

(define xcons mcons)

(define (acons x)
  (or (mpair? x)
      (pair? x)
      (and (syntax? x) (acons (syntax-e x)))))

(define (xcar x)
  (cond ((or (null? x) (eq? x 'nil))
         'nil)
        ((pair? x)
         (car x))
        ((mpair? x)
         (mcar x))
        ((syntax? x)
         (xcar (syntax-e x)))
        (else
         (error "Can't take car of" x))))

(define (xcdr x)
  (cond ((or (null? x) (eq? x 'nil))
         'nil)
        ((pair? x)
         (cdr x))
        ((mpair? x)
         (mcdr x))
        ((syntax? x)
         (xcdr (syntax-e x)))
        (else
         (error "Can't take cdr of" x))))

(define (ar-is x y)
  (or (eqv? x y)
      (and (eq? x 'nil) (eq? y '()))
      (and (eq? x '())  (eq? y 'nil))
      (and (string? x) (string? y) (string=? x y))
      (and (syntax? x) (syntax? y) (ar-is (syntax-e x) (syntax-e y)))
      (and (syntax? x) (ar-is (syntax-e x) y))
      (and (syntax? y) (ar-is x (syntax-e y)))))

(define (ar-srcloc loc x)
  (cond ((not (syntax? loc))
         x)
        ((syntax? x)
         (ar-srcloc loc (syntax-e x)))
        (else
         (datum->syntax #f x loc))))

(struct hide (v) #:transparent)

(define unhide hide-v)

(define (builtin-loc x)
  (if (syntax? x)
       (source-location->string x)
       'nil))

; deep convert Arc to Racket, also stripping syntax

(define (ar-denil x)
  (cond ((mpair? x)
         (cons (ar-denil-car (mcar x))
               (ar-denil-cdr (mcdr x))))
        ((pair? x)
         (cons (ar-denil-car (car x))
               (ar-denil-cdr (cdr x))))
        ((syntax? x)
         (ar-denil (syntax-e x)))
        (else
         x)))

(define (ar-denil-car x)
  (cond ((eq? x 'nil)
         'nil)
        ((null? x)
         'nil)
        ((syntax? x)
         (ar-denil-car (syntax-e x)))
        (else
         (ar-denil x))))

(define (ar-denil-cdr x)
  (cond ((eq? x 'nil)
         '())
        ((null? x)
         '())
        ((syntax? x)
         (ar-denil-cdr (syntax-e x)))
        (else
         (ar-denil x))))

(define (deep-unwrap x)
  (ar-denil x))

(define hasloc syntax?)
