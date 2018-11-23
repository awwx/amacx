#lang racket

(require racket/random)
(require "uniq.rkt")
(require "symtab.rkt")

(provide builtins ar-amac r-apply ar-nillist ar-niltree ar-rep ar-true?
         ref sref)

(define (ar-denillist x)
  (cond ((mpair? x)
         (cons (mcar x)
               (ar-denillist (mcdr x))))
        ((eq? x 'nil)
         '())
        (else x)))

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

(define (ar-nil-terminate l)
  (if (or (eqv? l '()) (eqv? l 'nil))
      '()
      (cons (car l) (ar-nil-terminate (cdr l)))))

(define (ar-apply-args args)
  (cond ((null? args) '())
        ((null? (cdr args)) (ar-nil-terminate (car args)))
        (#t (cons (car args) (ar-apply-args (cdr args))))))

(define (ar-nil x)
  (if (or (eq? x '()) (eq? x #f))
      'nil
      x))

(define (tnil x) (if x 't 'nil))

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

(define (sref g key val)
  (cond ((hash? g)  (if (eq? val 'nil)
                          (hash-remove! g key)
                          (hash-set! g key val)))
        ((symtab? g) (symtab-set! g key val))
        ((string? g) (string-set! g key val))
        ((pair? g)   (nth-set! g key val))
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
        (else
         (err "Function call on inappropriate object" fn args))))

(define (combine-apply args)
  (cond ((null? args)
         '())
        ((null? (cdr args))
         (ar-denillist (car args)))
        (else
         (cons (car args) (combine-apply (cdr args))))))

; where args is an Arc list

(define ar-apply
  (case-lambda
    ((fn)       (r-apply fn '()))
    ((fn args)  (r-apply fn (ar-denillist args)))
    ((fn . rest) (r-apply fn (combine-apply rest)))))

(define (protect during after)
  (dynamic-wind (lambda () #t) during after))

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(define (atomic-invoke f)
  (if (thread-cell-ref ar-sema-cell)
      (ar-apply f '())
      (begin
        (thread-cell-set! ar-sema-cell #t)
        (protect
          (lambda ()
            (call-with-semaphore
             ar-the-sema
             (lambda () (ar-apply f '()))))
          (lambda ()
            (thread-cell-set! ar-sema-cell #f))))))

(define builtin-table (make-hash))

(define-syntax b=
  (syntax-rules ()
    ((b= name val)
     (hash-set! builtin-table 'name val))))

(define-syntax bdef
  (syntax-rules ()
    ((builtin name args body ...)
     (b= name
       (procedure-rename (λ args body ...) 'name)))))

(define (builtins)
  (hash-copy builtin-table))

(define err error)

(define (nth-set! lst n val)
  (set-mcar! (list-tail lst n) val))

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

(define (ar-amac x)
  (eq? (ar-tag-type x) 'mac))

(bdef acons (x)
  (tnil (mpair? x)))

(b= annotate ar-tag)

(bdef ar-assert (x)
  (if (not (ar-false? x))
       (printf "OK\n")
       (begin (printf "FAIL\n")
              (error 'fail))))

(b= ar-apply ar-apply)

(b= apply ar-apply)

(bdef ar-disp (x port)
  (display (ar-denil x) port))

(bdef ar-write (x port)
  (write (ar-denil x) port))

(bdef ar-iso (a b)
  (tnil (equal? a b)))

(bdef is2 (a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            (and (ar-false? a) (ar-false? b)))))

(b= ar-strlen string-length)

(b= ar-str-append string-append)

(b= ar-symstr symbol->string)

(b= ar-tag-type ar-tag-type)

(b= ar-uniq ar-uniq)

(bdef ar-<2 (x y)
  (tnil (cond ((and (number? x) (number? y)) (< x y))
              ((and (string? x) (string? y)) (string<? x y))
              ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x)   (char? y)) (char<? x y))
              (else (< x y)))))

(b= atomic-invoke atomic-invoke)

(bdef a-char (x)
  (tnil (char? x)))

(bdef a-fn (x)
  (tnil (procedure? x)))

(bdef a-num (x)
  (tnil (number? x)))

(bdef a-socket (x)
  (tnil (tcp-listener? x)))

(bdef a-str (x)
  (tnil (string? x)))

(bdef a-sym (x)
  (tnil (symbol? x)))

(bdef a-table (x)
  (tnil (hash? x)))

(bdef a-tagged (x)
  (tnil (ar-tagged? x)))

(bdef a-thread (x)
  (tnil (thread? x)))

(bdef an-exception (x)
  (tnil (exn? x)))

(bdef an-input (x)
  (tnil (input-port? x)))

(bdef an-int (x)
  (tnil (and (integer? x) (exact? x))))

(bdef an-output (x)
  (tnil (output-port? x)))

(bdef car (x)
  (cond ((mpair? x)
         (mcar x))
        ((eq? x 'nil)
         'nil)
        (else
         (err "Can't take car of" x))))

(bdef cdr (x)
  (cond ((mpair? x)
         (mcdr x))
        ((eq? x 'nil)
         'nil)
        (else
         (err "Can't take cdr of" x))))

(bdef cons (a d)
  (mcons a d))

(b= err error)

(bdef fnname (fn)
  (object-name fn))

(bdef has (g k)
  (cond ((hash? g)
         (tnil (hash-has-key? g k)))
        ((symtab? g)
         (tnil (symtab-has-key? g k)))
        (else (error "has: not a table" g))))

(b= msec current-milliseconds)

(bdef namefn (name fn)
  (procedure-rename fn name))

(b= protect protect)

(b= rep ar-rep)

(define (wrapnil f) (lambda args (apply f args) 'nil))

(b= sleep (wrapnil sleep))

(b= stdin  current-input-port)
(b= stderr current-error-port)
(b= stdout current-output-port)

(b= sref sref)

(b= symtab new-symtab)

(b= t 't)

(bdef table ()
  (make-hash))

(bdef table-each (g f)
  (cond ((symtab? g)
         (symtab-each g f))
        ((hash? g)
         (hash-for-each g f))
        (else
         (err "not a table" g)))
  g)

(b= thread thread)

(b= thread-wait thread-wait)

(b= ar-writec write-char)

(b= + +)
(b= - -)
(b= * *)
(b= / /)

(module+ test (require rackunit/chk)
  (let ((g (make-hash))
        (sref (hash-ref builtin-table 'sref)))
    (sref g 'a 1)
    (check-equal? (hash-ref g 'a) 1)))
