#lang racket

(provide w/prefix)

(define (w/prefix prefix thunk)
  (define-values (in out) (make-pipe 100))
  (define th
    (thread
      (λ ()
        (let loop ((at-beginning-of-line #t))
          (let ((c (read-char in)))
            (cond ((eof-object? c)
                   (flush-output)
                   ; return, which shuts down the thread
                   (void))
                  (else
                   ; TODO errors writing to stdout (e.g. redirecting
                   ; to a pipe in the shell which is closed) should
                   ; propagate back to the caller instead of being
                   ; ignored.
                   (when at-beginning-of-line
                     (display prefix))
                   (write-char c)
                   (loop (eqv? c #\newline)))))))))
  (dynamic-wind
    (λ () (void))
    (λ ()
      (parameterize ((current-output-port out))
        (thunk)))
    (λ ()
      ; closing the output port causes (read-char in) to return eof
      (close-output-port out)
      (thread-wait th))))
