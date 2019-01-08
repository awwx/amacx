#lang racket

(require syntax/srcloc)

(provide splicing-port with-blockstr-readtable)

; A block-string line consists of:
;  - Zero or more spaces or tabs at the start of the line
;  - The Unicode "Box Drawings Light Vertical" character (U+2502)
;  - A single space
;  - and the rest of the line, which makes up the string we want to capture.
;
; For easy extraction, use a capture group (...) for "the rest of the
; line".

(define blockstr-regexp #px"^[ \t]*│ ([^\r\n]*)(\r\n|\n)?")

(define (splicing-port source-port)
  (define at-eof #f)
  (define bytes-and-locs '())

  ; location is (line-number column-number pos)
  (define location '(1 0 1))

  (define (read-next-char)
    (let ((c (read-char source-port)))
      (cond ((eof-object? c)
             (set! at-eof #t))
            (else
             (let ((loc (call-with-values
                          (λ () (port-next-location source-port))
                          list)))
               (set! bytes-and-locs
                 (append bytes-and-locs
                         (list `(loc ,@loc))
                         (map (λ (byte)
                                `(byte ,byte))
                              (bytes->list
                                (string->bytes/utf-8
                                  (string c)))))))))))

  (define (fill-buffer buffer)
    (cond ((and (null? bytes-and-locs) at-eof)
           eof)
          ((null? bytes-and-locs)
           (read-next-char)
           (fill-buffer buffer))
          ((eqv? (caar bytes-and-locs) 'loc)
           (set! location (cdar bytes-and-locs))
           (set! bytes-and-locs (cdr bytes-and-locs))
           (fill-buffer buffer))
          (else
           (let ((b (cadar bytes-and-locs)))
             (set! bytes-and-locs (cdr bytes-and-locs))
             (bytes-set! buffer 0 b)
             1))))

  (define (peek-buffer buffer skip offset)
    (let ((here (list-tail bytes-and-locs offset)))
      (cond ((and (null? here) at-eof)
             eof)
            ((null? here)
             (read-next-char)
             (peek-buffer buffer skip offset))
            ((eqv? (caar here) 'loc)
             (peek-buffer buffer skip (+ offset 1)))
            ((> skip 0)
             (peek-buffer buffer (- skip 1) (+ offset 1)))
            (else
             (bytes-set! buffer 0 (cadar here))
             1))))

  (define port
    (make-input-port
      ; name
      (object-name source-port)

      ; read-in
      (λ (buffer)
        ; (printf "charport read-in request ~a~n" (bytes-length buffer))
        (let ((result (fill-buffer buffer)))
          ; (printf "charport read-in result ~s ~a~n" buffer result)
          result))

      ; peek
      (λ (buffer skip progress-evt)
        ; (printf "peek request n: ~a, skip: ~a~n" (bytes-length buffer) skip)
        (unless (eq? progress-evt #f)
          (error "oops not implemented"))
        (let ((n (peek-buffer buffer skip 0)))
          ; (printf "peek result: ~a ~s~n" n buffer)
          n))

      ; close
      (λ ()
        (printf "closed~n"))

      ; get-progress-evt
      #f

      ; commit
      #f

      ; get-location
      (λ ()
        ; (printf "charport get-location: ~s~n" location)
        (apply values location))))

  (define (on-section-sign)
    ; Capture our current position (one character past reading the section
    ; sign).
    (define here location)

    ; Peek from our splicing port up to and including the eol,
    ; populating our buffer.

    (regexp-match-peek #px"\n" port)

    ; At this point bytes-and-locs has been populated through to the
    ; end of the line (ready for Racket to read), and we've read and
    ; consumed the eol from source-port.

    ; Now read the block strings from the *source port* (where they'll
    ; never even be seen by the splicing port).

    (define s
      (let loop ((accum ""))
        (let ((result (regexp-try-match blockstr-regexp source-port)))
          (cond ((eq? result #f)
                 ; no match, all done
                 accum)
                (else
                 (let ((str (bytes->string/utf-8 (second result))))
                   (loop (if (equal? accum "")
                             str
                             (string-append accum "\n" str)))))))))

    ; Now we've read and consumed from the source-port the block
    ; string line.

    (datum->syntax #f s
      (list (object-name source-port)    ; name
            (first here)                 ; line
            (- (second here) 1)          ; column (adjust)
            (- (third here) 1)           ; pos (adjust)
            1)))                         ; span

  (port-count-lines! port)

  (values on-section-sign port))

(define (blockstr-readtable base-readtable on-section-sign)
  (make-readtable base-readtable #\§ 'non-terminating-macro
    (case-lambda
      [(ch in)
        (syntax->datum (on-section-sign))]
      [(ch in src line col pos)
        (on-section-sign)])))

(define (with-blockstr-readtable base-readtable in f)
  (let-values (((on-section-sign port) (splicing-port in)))
    (port-count-lines! port)
    (parameterize ((current-readtable
                    (blockstr-readtable base-readtable on-section-sign)))
      (f port))))
