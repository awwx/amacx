#lang racket

(require "symtab.rkt")

(provide ar-false? ar-nillist ar-niltree ar-true? err ref sref
         r-apply)

(define err error)

(define (ar-false? x)
  (or (eq? x 'nil) (eq? x #f)))

(define (ar-true? x)
  (not (ar-false? x)))

(define (ar-nillist x)
  (cond ((pair? x)
         (mcons (car x)
                (ar-nillist (cdr x))))

        ((null? x)
         'nil)

        (else x)))

(define (ar-niltree x)
  (cond ((pair? x)
         (mcons (ar-niltree (car x))
                (ar-niltree (cdr x))))

        ((or (eq? x #f) (eq? x '()))
         'nil)

        (else x)))

(define ref
  (case-lambda
   ((g k)
    (cond ((hash? g)
           (hash-ref g k 'nil))
          ((symtab? g)
           (symtab-ref g k))
          (else (error "can't ref non-table" g))))

   ((g k default)
    (cond ((hash? g)
           (hash-ref g k default))
          ((symtab? g)
           (if (symtab-has-key? g k)
               (symtab-ref g k)
               default))
          (else (error "can't ref non-table" g))))))

(define (nth-set! lst n val)
  (set-mcar! (list-tail lst n) val))

(define (sref g key val)
  (cond ((hash? g)  (if (eq? val 'nil)
                          (hash-remove! g key)
                          (hash-set! g key val)))
        ((symtab? g) (symtab-set! g key val))
        ((string? g) (string-set! g key val))
        ((pair? g)   (nth-set! g key val))
        ((namespace? g)
         (namespace-set-variable-value! key val #f g))
        (else (err "Can't set reference " g key val)))
  val)

; where args is a racket list

(define (r-apply fn args)
  (cond ((procedure? fn)
         (apply fn args))
        ((pair? fn)
         (list-ref fn (car args)))
        ((string? fn)
         (string-ref fn (car args)))
        ((hash? fn)
         (hash-ref fn
                   (car args)
                   (λ ()
                     (if (pair? (cdr args)) (cadr args) 'nil))))
        ((symtab? fn)
         (symtab-ref fn (car args)))
        ((namespace? fn)
         (namespace-variable-value (car args) #t #f fn))
        (else
         (err "Function call on inappropriate object" fn args))))
