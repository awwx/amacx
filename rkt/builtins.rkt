#lang racket

(require racket/hash)
(require racket/random)
(require "ail-ns.rkt")
(require "aload.rkt")
(require "data.rkt")
(require "symtab.rkt")
(require "uniq.rkt")

(provide builtins)

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

(define (ar-nil x)
  (if (or (eq? x '()) (eq? x #f))
      'nil
      x))

(define (tnil x) (if x 't 'nil))

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

(define builtins #f)

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

(bdef acons (x)
  (tnil (mpair? x)))

(b= ail-namespace ail-ns)

(b= annotate ar-tag)

(bdef ar-assert (x)
  (if (not (ar-false? x))
       (printf "OK\n")
       (begin (printf "FAIL\n")
              (err 'fail))))

(b= ar-apply ar-apply)

(b= apply ar-apply)

(bdef ar-builtins ()
  builtins)

(bdef ar-disp (x port)
  (display (ar-denil x) port))

(bdef ar-write (x port)
  (write (ar-denil x) port))

(bdef ar-iso (a b)
  (tnil (equal? a b)))

(b= ar-load aload)

(bdef ar-print args
  (for ((arg args))
    (write arg)
    (display " "))
  (display "\n"))

(bdef is2 (a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            (and (ar-false? a) (ar-false? b)))))

(b= ar-strlen string-length)

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

(bdef ar->2 (x y)
  (tnil (cond ((and (number? x) (number? y)) (> x y))
              ((and (string? x) (string? y)) (string>? x y))
              ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x)   (char? y)) (char>? x y))
              (else (> x y)))))

(b= atomic-invoke atomic-invoke)

(bdef a-char (x)
  (tnil (char? x)))

(bdef a-fn (x)
  (tnil (procedure? x)))

(bdef a-num (x)
  (tnil (number? x)))

(bdef iround (x)
  (inexact->exact (round x)))

(bdef a-socket (x)
  (tnil (tcp-listener? x)))

(bdef a-str (x)
  (tnil (string? x)))

(bdef a-sym (x)
  (tnil (symbol? x)))

(bdef a-table (x)
  (tnil (or (hash? x)
            (symtab? x)
            (namespace? x))))

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

(b= charcode char->integer)

(b= charstr string)

; todo sockets, custodians
;
(bdef close (port)
  (cond ((input-port? port) (close-input-port port))
        ((output-port? port) (close-output-port port))
        (else (err "can't close" port))))

(bdef cons (a d)
  (mcons a d))

(b= err err)

(bdef fnname (fn)
  (object-name fn))

(define missing (list 'missing))

(bdef has (g k)
  (cond ((hash? g)
         (tnil (hash-has-key? g k)))
        ((symtab? g)
         (tnil (symtab-has-key? g k)))
        ((namespace? g)
         (tnil (not (eq? missing
                         (namespace-variable-value k #t (λ () missing) g)))))
        (else (err "has: not a table" g))))

(b= infile open-input-file)

(bdef inspect (x)
  (let ((p (open-output-string)))
    (write x p)
    (get-output-string p)))

(b= instring open-input-string)

(b= mod modulo)

(b= msec current-milliseconds)

(bdef namefn (name fn)
  (procedure-rename fn name))

(b= numstr number->string)

(bdef open-socket (num)
  (tcp-listen num 50 #t))

(bdef open-output-file (name mode exists)
  (open-output-file name #:mode mode #:exists exists))

(b= outstring open-output-string)

(b= protect protect)

(b= racket-eval eval)

(bdef readport (port eof)
  (let ((expr (read port)))
    (if (eof-object? expr) eof expr)))

(b= rep ar-rep)

(define (wrapnil f) (lambda args (apply f args) 'nil))

(b= sleep (wrapnil sleep))

(b= sref sref)

(b= stdin  current-input-port)
(b= stderr current-error-port)
(b= stdout current-output-port)

(bdef strnum (s radix)
  (let ((r (string->number s radix)))
    (if r r 'nil)))

(bdef strchars (x)
  (ar-nillist (string->list x)))

(b= strsym string->symbol)

(b= symstr symbol->string)

(b= symtab new-symtab)

(b= str-append string-append)

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


(set! builtins
  (hash-union (hash) builtin-table))

(module+ test (require rackunit/chk)
  (let ((g (make-hash))
        (sref (hash-ref builtin-table 'sref)))
    (sref g 'a 1)
    (check-equal? (hash-ref g 'a) 1)))
