#lang racket

(provide quote-xVrP8JItk2Ot
         fn-xVrP8JItk2Ot
         assign-xVrP8JItk2Ot
         if-xVrP8JItk2Ot
         call-xVrP8JItk2Ot
         ns-var-xVrP8JItk2Ot
         #%top)

(require (only-in "data.rkt" r-apply ar-nillist ar-true?))
(require (only-in "symtab.rkt" symtab? symtab-ref))

(define-syntax quote-xVrP8JItk2Ot
  (syntax-rules ()
    ((quote-xVrP8JItk2Ot x)
     'x)))

(define-syntax fn-xVrP8JItk2Ot
  (syntax-rules ()
    ((fn-xVrP8JItk2Ot (arg ...) e ...)
     (lambda (arg ...) e ...))

    ((fn-xVrP8JItk2Ot (arg ... . rest) e ...)
     (lambda (arg ... . racket-rest)
       (let ((rest (ar-nillist racket-rest)))
         e ...)))))

(define-syntax if-xVrP8JItk2Ot
  (syntax-rules ()
    ((if-xVrP8JItk2Ot e1 e2 e3)
     (if (ar-true? e1) e2 e3))))

(define-syntax assign-xVrP8JItk2Ot
  (syntax-rules ()
    ((assign-xVrP8JItk2Ot v x)
     (let ((val x))
       (set! v val)
       val))))

(define err error)

(define (funcall0 fn)
  (if (procedure? fn)
      (fn)
      (r-apply fn (list))))

(define (funcall1 fn arg1)
  (cond ((procedure? fn)
         (fn arg1))
        ((symtab? fn)
         (symtab-ref fn arg1))
        (else
         (r-apply fn (list arg1)))))

(define (funcall2 fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      (r-apply fn (list arg1 arg2))))

(define (funcall3 fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      (r-apply fn (list arg1 arg2 arg3))))

(define (funcall4 fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      (r-apply fn (list arg1 arg2 arg3 arg4))))

(define-syntax call-xVrP8JItk2Ot
  (syntax-rules ()
    ((call-xVrP8JItk2Ot f)
     (funcall0 f))

    ((call-xVrP8JItk2Ot f arg1)
     (funcall1 f arg1))

    ((call-xVrP8JItk2Ot f arg1 arg2)
     (funcall2 f arg1 arg2))

    ((call-xVrP8JItk2Ot f arg1 arg2 arg3)
     (funcall3 f arg1 arg2 arg3))

    ((call-xVrP8JItk2Ot f arg1 arg2 arg3 arg4)
     (funcall4 f arg1 arg2 arg3 arg4))

    ((call-xVrP8JItk2Ot f e ...)
     (r-apply f (list e ...)))))

(define-syntax ns-var-xVrP8JItk2Ot
  (syntax-rules ()
    ((ns-var-xVrP8JItk2Ot v)
     v)))

(module+ test (require rackunit/chk)
  (chk ((fn-xVrP8JItk2Ot (a b) (call-xVrP8JItk2Ot list a b)) 1 2)
       '(1 2)

       ((fn-xVrP8JItk2Ot args args) 1 2)
       (mcons 1 (mcons 2 'nil))

       ((fn-xVrP8JItk2Ot (a . rest) (call-xVrP8JItk2Ot list a rest)) 1 2 3)
       (list 1 (mcons 2 (mcons 3 'nil)))

       ((fn-xVrP8JItk2Ot (a b . rest) (call-xVrP8JItk2Ot list a b rest)) 1 2 3)
       (list 1 2 (mcons 3 'nil))))
