#lang racket

(require "parsing.rkt")

(provide square-readtable)

(define not-closed-msg "opening '[' not terminated with a closing ']'")

(define (read-square ch in src line col pos)
  (define (read-expr)
    (read-syntax/recursive (object-name in) in))

  (define result '())

  (define (emit x)
    (set! result (cons x result)))

  (define (after-open-or-value)
    (skip-whitespace in)
    (not-at-eof in not-closed-msg)

    (if (at-char in #\])
        'all-done
        (begin (emit (read-expr))
               (after-open-or-value))))

  (after-open-or-value)
  (datum->syntax #f `(square-bracket ,@(reverse result))))

(define (square-readtable base)
  (make-readtable base #\[ 'terminating-macro read-square))

(module+ test (require rackunit/chk)
  (define (parse s)
    (let ((in (open-input-string s)))
      (parameterize ((current-readtable (square-readtable #f)))
        (read in))))

  (chk (parse "[]")        '(square-bracket)
       (parse "[   ]")     '(square-bracket)
       (parse "[a]")       '(square-bracket a)
       (parse "[ a ]")     '(square-bracket a)
       (parse "[a b]")     '(square-bracket a b)
       (parse "[a b c]")   '(square-bracket a b c)
       (parse "[a [b] c]") '(square-bracket a (square-bracket b) c)))
