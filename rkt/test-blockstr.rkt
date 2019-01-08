#lang racket

; Test that objects are read the same using the splicing port
; as they are using Racket’s standard `read-syntax`.

(require "blockstr.rkt")
(require "dump-srcloc.rkt")

(define (dump-all in)
  (let loop ()
    (let ((x (read-syntax (object-name in) in)))
      (unless (eof-object? x)
        (dump-srcloc x)
        (loop))))
  (printf "eof~n"))

(define (tostring f)
  (let ((out (open-output-string)))
    (parameterize ((current-output-port out))
      (f))
    (get-output-string out)))

(define (dump-to-str in)
  (tostring (λ () (dump-all in))))

(define (one s)
  (let ((p (open-input-string s)))
    (port-count-lines! p)
    (dump-to-str p)))

(define (two s)
  (let ((p (open-input-string s)))
    (port-count-lines! p)
    (let-values (((on-section-sign port) (splicing-port p)))
      (dump-to-str port))))

(define (equals input a b)
  (cond ((equal? a b)
         (printf "OK~n~a~n~a~n" input a))
        (else
         (printf "FAIL~n~a~n~nResult:~n~a~nNot the expected:~n~a~n" input a b))))

(define (check s)
  (equals s (one s) (two s)))

(check "")
(check "a")
(check "ab")
(check "abc")

(check #<<END
foo bar λ §
three four
END
)

(check #<<END
(a b c)
END
)

(check #<<END
#hasheq((a . 5) (b . 7))
END
)


(define (test-blockstr s expected)
  (equals s
    (let ((p (open-input-string s)))
      (port-count-lines! p)
      (with-blockstr-readtable #f p
        (λ (in)
          (dump-to-str in))))
    expected))

(test-blockstr #<<END
one § two
  │ abc
  │ def
foo bar
END
  #<<END
string:1.0 (span 3) one
string:1.4 (span 1) "abc\ndef"
string:1.6 (span 3) two
string:4.0 (span 3) foo
string:4.4 (span 3) bar
eof

END
  )
