#lang racket

(require "parsing.rkt")

(provide curly-readtable)

(define curly-not-closed-msg
  "opening '{' not terminated with a closing '}'")

(define missing-comma-msg
  "value after key in {...} must be followed by a ',' or closing '}'")

(define (curly-not-at-eof in)
  (not-at-eof in curly-not-closed-msg))

(define (read-curly ch in src line col pos)
  (define (read-expr)
    (read-syntax/recursive #f in))

  (define result '())

  (define (emit x)
    (set! result (append result (list x))))

  (define (after-opening-bracket-or-comma-separator)
    (skip-whitespace in)
    (curly-not-at-eof in)
    (if (at-char in #\})
        'all-done!
        (after-key (read-expr))))

  (define (after-key k)
    (curly-not-at-eof in)

    ; Here we *don't* skip whitespace because a comma must immediately
    ; follow a key expression to act as a separator.  "{ a, b }"
    ; parses differently than "{ a ,b }".

    (cond ((at-char in #\,)
           (emit (list k))
           (after-opening-bracket-or-comma-separator))
          (else
           (after-key-without-comma k))))

  (define (after-key-without-comma k)
    (skip-whitespace in)
    (curly-not-at-eof in)
    (cond ((at-char in #\})
           (emit (list k))
           'all-done!)
          (else
           (emit (list k (read-expr)))
           (after-value))))

  (define (after-value)
    (curly-not-at-eof in)

    ; Another place where we don't skip whitespace because a comma
    ; must also immediately follow a value expression to act as a
    ; separator.

    (if (at-char in #\,)
        (after-opening-bracket-or-comma-separator)
        (after-value-without-comma)))

  (define (after-value-without-comma)
    (skip-whitespace in)
    (curly-not-at-eof in)
    (if (at-char in #\})
        'all-done!
        (error missing-comma-msg)))

  (after-opening-bracket-or-comma-separator)

  (datum->syntax #f `(curly-bracket ,@result)))


(define (curly-readtable base)
  (make-readtable base #\{ 'terminating-macro read-curly))


; (module+ test (require rackunit/chk)
;
;   (define (readall in)
;     (let loop ((accum '()))
;       (let ((v (read-syntax #f in)))
;         (if (eof-object? v)
;           (reverse accum)
;           (loop (cons v accum))))))
;
;   (define (parse s)
;     (parameterize ((current-readtable curly-readtable))
;       (let ((in (open-input-string s)))
;         (port-count-lines! in)
;         (map syntax->datum (readall in)))))
;
;   (chk (parse "")  '()
;        (parse "a")  '(a)
;
;        #:exn (parse "{")    curly-not-closed-msg
;        #:exn (parse "{  ")  curly-not-closed-msg
;
;        (parse "{}")    '( (curly-bracket) )
;        (parse "{  }")  '( (curly-bracket) )
;
;        (parse "{} x")   '( (curly-bracket) x )
;        (parse "{  } x") '( (curly-bracket) x )
;
;        (parse "{a}")    '( (curly-bracket (a)) )
;        (parse "{ a }")  '( (curly-bracket (a)) )
;
;        #:exn (parse "{a")    curly-not-closed-msg
;        #:exn (parse "{ a ")  curly-not-closed-msg
;
;        (parse "{a,}")    '( (curly-bracket (a)) )
;        (parse "{ a, }")  '( (curly-bracket (a)) )
;
;        #:exn (parse "{a,")    curly-not-closed-msg
;        #:exn (parse "{ a, ")  curly-not-closed-msg
;
;        #:exn (parse "{a ,}")  "unexpected `}`"
;
;        (parse "{a,b}")       '( (curly-bracket (a) (b)) )
;        (parse "{a,b,c}")     '( (curly-bracket (a) (b) (c)) )
;        (parse "{ a, b, c }") '( (curly-bracket (a) (b) (c)) )
;
;        (parse "{ a ,b }")    '( (curly-bracket (a ,b)) )
;
;        (parse "{ a 1 }")       '( (curly-bracket (a 1)) )
;        (parse "{ a 1, b }")    '( (curly-bracket (a 1) (b)) )
;        (parse "{ a, b 2 }")    '( (curly-bracket (a) (b 2)) )
;        (parse "{ a 1, b 2 }")  '( (curly-bracket (a 1) (b 2)) )
;
;        #:exn (parse "{ a b c }")  missing-comma-msg
;
;        (parse "{ a (+ 3 4), b 2 }")  '( (curly-bracket (a (+ 3 4)) (b 2)) )))
