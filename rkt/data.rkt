#lang racket

(require "symtab.rkt")

(provide ar-denil ar-false? ar-tagged? ar-tag-type ar-tag ar-rep
         ar-nillist ar-niltree ar-true? err ref sref r-apply)

(define err error)

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

(define (ar-false? x)
  (or (eq? x 'nil) (eq? x #f)))

(define (ar-true? x)
  (not (ar-false? x)))

(define (ar-tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (ar-tag-type x)
  (and (ar-tagged? x) (vector-ref x 1)))

(define (ar-tag type rep)
  (if (eq? (ar-tag-type rep) type)
       rep
       (vector 'tagged type rep)))

(define (ar-rep x)
  (if (ar-tagged? x) (vector-ref x 2) x))

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
          ((namespace? g)
           (namespace-variable-value k #t #f g))
          (else (error "can't ref non-table" g))))

   ((g k default)
    (cond ((hash? g)
           (hash-ref g k default))
          ((symtab? g)
           (if (symtab-has-key? g k)
               (symtab-ref g k)
               default))
          ((namespace? g)
           (namespace-variable-value k #t (λ () default) g))
          (else (error "can't ref non-table" g))))))

(define (mlist-tail lst n)
  (if (= n 0)
       lst
       (mlist-tail (mcdr lst) (- n 1))))

(define (nth-set! lst n val)
  (set-mcar! (mlist-tail lst n) val))

(define (sref g key val)
  (cond ((hash? g)   (if (eq? val 'nil)
                          (hash-remove! g key)
                          (hash-set! g key val)))
        ((symtab? g) (symtab-set! g key val))
        ((string? g) (string-set! g key val))
        ((mpair? g)  (nth-set! g key val))
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
         (if (pair? (cdr args))
             (symtab-ref-default fn (car args) (cadr args))
             (symtab-ref fn (car args))))
        ((namespace? fn)
         (namespace-variable-value (car args) #t #f fn))
        (else
         (err "Function call on inappropriate object" fn args))))
