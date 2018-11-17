#lang racket

(require racket/random)

(provide ar-uniq)

; avoid characters that can be hard to distinguish in some fonts: 0 and O; 1 and I

(define (rand-string gen n)
  (list->string
    (cons (random-ref "abcdefghijklmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ" gen)
          (random-sample "abcdefghijklmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ23456789" (- n 1) gen #:replacement? #t))))

(define (ar-uniq gen sym)
  (let ((r (rand-string (if (eq? gen 'nil) (current-pseudo-random-generator) gen)
                        16)))
    (if (eq? sym 'nil)
        (string->symbol r)
        (string->symbol (string-append (symbol->string sym) "-" r)))))
