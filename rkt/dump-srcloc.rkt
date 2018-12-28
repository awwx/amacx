#lang racket

(require syntax/srcloc)

(provide dump-srcloc)

(define (prindent indent)
  (unless (= indent 0)
    (display " ")
    (prindent (- indent 1))))

(define (dump-srcloc x (indent 0))
  (cond ((syntax? x)
         (prindent indent)
         (display (source-location->string x))
         (display " (span ")
         (write (syntax-span x))
         (display ") ")
         (write (syntax->datum x))
         (display "\n")
         (cond ((hash? (syntax-e x))
                (for ((key (hash-keys (syntax-e x))))
                  (dump-srcloc (hash-ref (syntax-e x) key) (+ indent 2))))
               (else
                (define ys (syntax->list x))
                (when ys
                  (for ((y ys))
                    (dump-srcloc y (+ indent 2)))))))
        (else
         (prindent indent)
         (write x)
         (display "\n"))))
