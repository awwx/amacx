#lang racket

(provide skip-whitespace not-at-eof at-char)

; From
; [https://docs.racket-lang.org/reference/readtables.html?q=read%2Frecursive#%28def._%28%28quote._~23~25kernel%29._readtable-mapping%29%29]
;
(define (skip-whitespace port)
  ; Skips whitespace characters, sensitive to the current
  ; readtable's definition of whitespace
  (let ([ch (peek-char port)])
    (unless (eof-object? ch)
      ; Consult current readtable:
      (let-values ([(like-ch/sym proc dispatch-proc)
                    (readtable-mapping (current-readtable) ch)])
        ; If like-ch/sym is whitespace, then ch is whitespace
        (when (and (char? like-ch/sym)
                   (char-whitespace? like-ch/sym))
          (read-char port)
          (skip-whitespace port))))))

(define (not-at-eof in msg)
  (when (eof-object? (peek-char in))
    (error msg)))

(define (at-char in char)
  (cond ((eq? (peek-char in) char)
         (read-char in)
         #t)
        (else #f)))
