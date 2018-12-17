#lang racket

(require syntax/location)

(provide runtimes runtimef)

(define runtimes '(mpair srcloc))

(define (runtimef runtime name)
  (dynamic-require (list 'submod (quote-module-path) runtime) name))

(define-syntax-rule (create-runtime name spec)
  (module name racket
    (require "common.rkt")
    (require "symtab.rkt")
    (require "uniq.rkt")
    (require spec)

    (provide (all-defined-out)
             (all-from-out spec))

    (define runtime 'name)

    (define (tnil x) (if x 't 'nil))

    (define (xcadr x)
      (xcar (xcdr x)))

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

    (define (ar-tag type rep)
      (if (eq? (unwrap (ar-tag-type rep)) (unwrap type))
           rep
           (vector 'tagged type rep)))

    (define (ar-tagged? x)
      (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

    (define (ar-tag-type x)
      (and (ar-tagged? x) (vector-ref x 1)))

    (define (ar-rep x)
      (if (ar-tagged? x) (vector-ref x 2) x))

    (define (err . args)
      (apply error (deep-unwrap args)))

    (define (xcaris x v)
      (and (acons x)
           (ar-is (xcar x) v)))

    ; This is like ar-denil except that quoted values are left unconverted.

    (define (arcail x)
      (cond ((xcaris x 'quote-xVrP8JItk2Ot)
             (ar-srcloc x (list 'quote-xVrP8JItk2Ot (hide (xcadr x)))))

            ((acons x)
             (ar-srcloc x
               (cons (arcail-car (xcar x))
                     (arcail-cdr (xcdr x)))))

            (else x)))

    (define (arcail-car x)
      (if (ar-is x 'nil)
          x
          (arcail x)))

    (define (arcail-cdr x)
      (if (ar-is x 'nil)
          '()
          (arcail x)))

    (define (eval-ail x namespace)
      (eval (arcail x) namespace))

    ; Would go in runtime specific module but don't have ar-niltree
    ; there.
    (define readport
      (case runtime
        ; For the mpair runtime, we want to use `read` and
        ; call `ar-niltree` on the result.
        ((mpair) (λ (port eof)
                   (let ((x (read port)))
                     (if (eof-object? x)
                          eof
                          (ar-niltree x)))))

        ; For srcloc, we want to use `read-syntax`, and to return
        ; the result without conversion.
        ((srcloc) (λ (port eof)
                    (let ((x (read-syntax (object-name port) port)))
                      (if (eof-object? x)
                           eof
                           x))))

        (else (err "unknown runtime" runtime))))

    (define (protect during after)
      (dynamic-wind (lambda () #t) during after))

    (define (builtin-ar-assert x)
      (if (not (ar-false? x))
           (printf "OK\n")
           (begin (printf "FAIL\n")
                  (error 'fail)))) ; TODO

    (define (builtin-ar-disp x port)
      (display (ar-denil x) port)
      (flush-output port)
      x)

    (define (builtin-ar-write x port)
      (write (ar-denil x) port)
      (flush-output port)
      x)

    (define (builtin-ar-writec c port)
      (write-char (unwrap c) port)
      (flush-output port)
      c)

    ; used by some early tests before iso is loaded
    (define (ar-iso x y)
      (or (ar-is x y)
          (and (acons x)
               (acons y)
               (ar-iso (xcar x) (xcar y))
               (ar-iso (xcdr x) (xcdr y)))))

    (define (builtin-ar-iso x y)
      (tnil (ar-iso x y)))

    (define (builtin-ar-print . args)
      (for ((arg args))
        (write arg)
        (display " "))
      (display "\n")
      (flush-output)
      (car args))

    (define (builtin-is2 a b)
      (tnil (ar-is a b)))

    (define (builtin-ar-strlen x)
      (string-length (unwrap x)))

    (define (builtin-ar-symstr x)
      (symbol->string (unwrap x)))

    (define (builtin-ar-uniq gen sym)
      (ar-uniq (unwrap gen) (unwrap sym)))

    (define (builtin-ar-<2 x y)
      (unwraps (x y)
        (tnil (cond ((and (number? x) (number? y)) (< x y))
                    ((and (string? x) (string? y)) (string<? x y))
                    ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                             (symbol->string y)))
                    ((and (char? x)   (char? y)) (char<? x y))
                    (else (< x y))))))

    (define (builtin-ar->2 x y)
      (unwraps (x y)
        (tnil (cond ((and (number? x) (number? y)) (> x y))
                    ((and (string? x) (string? y)) (string>? x y))
                    ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                             (symbol->string y)))
                    ((and (char? x)   (char? y)) (char>? x y))
                    (else (> x y))))))

    (define (builtin-call-w/stdout port thunk)
      (parameterize ((current-output-port port)) (thunk)))

    ; TODO sockets, custodians
    (define (builtin-close port)
      (cond ((input-port? port) (close-input-port port))
            ((output-port? port) (close-output-port port))
            (else (err "can't close" port))))

    (define (disp-to-string x)
      (let ((p (open-output-string)))
        (display x p)
        (get-output-string p)))

    (define (inspect x)
      (let ((p (open-output-string)))
        (write x p)
        (get-output-string p)))

    ; TODO maybe for eval we don't need to strip syntax?
    (define (builtin-eval-racket x (ns (make-base-namespace)))
      (eval (ar-denil x) ns))

    (define (xlist-ref xs n)
      (if (= n 0)
           (xcar xs)
           (xlist-ref (xcdr xs) (- n 1))))

    ; where args is a Racket list
    (define (r-apply fn args)
      (cond ((procedure? fn)
             (apply fn args))
            ((acons fn)
             (xlist-ref fn (unwrap (car args))))
            ((string? fn)
             (string-ref (unwrap fn) (unwrap (car args))))
            ((hash? fn)
             (hash-ref fn
                       (deep-unwrap (car args))
                       (λ ()
                         (if (pair? (cdr args)) (cadr args) 'nil))))
            ((symtab? fn)
             (if (pair? (cdr args))
                 (symtab-ref-default fn (deep-unwrap (car args)) (cadr args))
                 (symtab-ref fn (deep-unwrap (car args)))))
            ((namespace? fn)
             (if (pair? (cdr args))
                 (namespace-variable-value
                   (unwrap (car args))     ; sym
                   #t             ; use-mapping?
                   (λ ()          ; failure-thunk
                     (cadr args))
                   fn)            ; namespace
                 (namespace-variable-value
                   (unwrap (car args))     ; sym
                   #t             ; use-mapping?
                   #f             ; failure-thunk
                   fn)))          ; namespace
            ((and (eq? runtime 'srcloc) (syntax? fn))
             (r-apply (unwrap fn) args))
            (else
             (err "Function call on inappropriate object" fn args))))

    ; e.g. (ref g k) or (ref g k default)
    (define (ref . args)
      (r-apply (car args) (cdr args)))

    (define missing (list 'missing))

    (define (has g k)
      (let ((k (deep-unwrap k)))
        (cond ((hash? g)
               (hash-has-key? g k))
              ((symtab? g)
               (symtab-has-key? g k))
              ((namespace? g)
               (not (eq? missing
                         (namespace-variable-value k #t (λ () missing) g))))
              (else (err "has: not a table" g)))))

    (define (aload name
                   target-module
                   (macro-module target-module))
      ((or (ref target-module 'load #f)
           (ref macro-module  'load))
       name
       target-module))

    ; shallow convert Arc list to Racket
    (define (ar-denillist x)
      (cond ((acons x)
             (cons (xcar x)
                   (ar-denillist (xcdr x))))
            ((ar-false? x)
             '())
            (else x)))

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
        ((fn)        (r-apply fn '()))
        ((fn args)   (r-apply fn (ar-denillist args)))
        ((fn . rest) (r-apply fn (combine-apply rest)))))

    (define ar-the-sema (make-semaphore 1))

    (define ar-sema-cell (make-thread-cell #f))

    (define (atomic-invoke f)
      (if (thread-cell-ref ar-sema-cell)
          (r-apply f '())
          (begin
            (thread-cell-set! ar-sema-cell #t)
            (protect
              (lambda ()
                (call-with-semaphore
                 ar-the-sema
                 (lambda () (r-apply f '()))))
              (lambda ()
                (thread-cell-set! ar-sema-cell #f))))))

    (define (caller f)
      (if (procedure? f)
           f
           (λ args
             (r-apply f args))))

    (define-syntax quote-xVrP8JItk2Ot
      (syntax-rules ()
        ((quote-xVrP8JItk2Ot x)
         (unhide 'x))))

    (define-syntax fn-xVrP8JItk2Ot
      (syntax-rules ()
        ((fn-xVrP8JItk2Ot (arg (... ...)) e (... ...))
         (lambda (arg (... ...)) e (... ...)))

        ((fn-xVrP8JItk2Ot (arg (... ...) . rest) e (... ...))
         (lambda (arg (... ...) . racket-rest)
           (let ((rest (ar-nillist racket-rest)))
             e (... ...))))))

    (define-syntax if-xVrP8JItk2Ot
      (syntax-rules ()
        ((if-xVrP8JItk2Ot e1 e2 e3)
         (if (ar-true? e1) e2 e3))))

    (define-syntax assign-xVrP8JItk2Ot
      (syntax-rules ()
        ((assign-xVrP8JItk2Ot v x)
         (let ((val x))
           (set! v val)
           val))))

    (define-syntax (funcall0 stx)
      (syntax-case stx ()
        ((funcall0 fn)
         (quasisyntax/loc stx
           (#,(syntax/loc #'fn (caller fn)))))))

    (define-syntax (funcall1 stx)
      (syntax-case stx ()
        ((funcall2 fn arg1)
         (quasisyntax/loc stx
           (#,(syntax/loc #'fn (caller fn)) arg1)))))

    (define-syntax (funcall2 stx)
      (syntax-case stx ()
        ((funcall2 fn arg1 arg2)
         (quasisyntax/loc stx
           (#,(syntax/loc #'fn (caller fn)) arg1 arg2)))))

    (define-syntax (funcall3 stx)
      (syntax-case stx ()
        ((funcall2 fn arg1 arg2 arg3)
         (quasisyntax/loc stx
           (#,(syntax/loc #'fn (caller fn)) arg1 arg2 arg3)))))

    (define-syntax (funcall4 stx)
      (syntax-case stx ()
        ((funcall4 fn arg1 arg2 arg3 arg4)
         (quasisyntax/loc stx
           (#,(syntax/loc #'fn (caller fn)) arg1 arg2 arg3 arg4)))))

  (define-syntax call-xVrP8JItk2Ot
    (syntax-rules ()
      ((call-xVrP8JItk2Ot f)
       (funcall0 f))

      ((call-xVrP8JItk2Ot f arg1)
       (funcall1 f arg1))

      ((call-xVrP8JItk2Ot f arg1 arg2)
       (funcall2 f arg1 arg2))

      ((call-xVrP8JItk2Ot f arg1 arg2 arg3)
       (funcall3 f arg1 arg2 arg3))

      ((call-xVrP8JItk2Ot f arg1 arg2 arg3 arg4)
       (funcall4 f arg1 arg2 arg3 arg4))

      ((call-xVrP8JItk2Ot f e (... ...))
       (r-apply f (list e (... ...))))))

    (define-syntax ns-var-xVrP8JItk2Ot
      (syntax-rules ()
        ((ns-var-xVrP8JItk2Ot v)
         v)))

    (define (sref g key val)
      (let ((key (deep-unwrap key)))
        (cond ((hash? g)   (if (eq? val 'nil)
                                (hash-remove! g key)
                                (hash-set! g key val)))
              ((symtab? g) (symtab-set! g key val))
              ((string? g) (string-set! g key val))
              ((mpair? g)  (nth-set! g key val))
              ((namespace? g)
               (namespace-set-variable-value! key val #f g))
              ; TODO err
              (else (error "Can't set reference " g key val)))
        val))

    (define (infile path)
      (let ((port (open-input-file (unwrap path))))
        (port-count-lines! port)
        port))

    (define (instring s)
      (let ((port (open-input-string (unwrap s))))
        (port-count-lines! port)
        port))

    (define (on-err errfn f)
      (with-handlers ((exn:fail? errfn))
        (f)))

    (define (wrapnil f)
      (λ args
        (apply f args) 'nil))

    (define (table-each g f)
      (cond ((hash? g)
             (hash-for-each g f))
            ((symtab? g)
             (symtab-each g f))
            (else
             (err "not a table" g)))
      g)

    (define (quote-this x)
      (hide (ar-niltree x)))

    (define runtime-builtins
      (hash
        'all-tests      (λ () (ar-niltree (all-tests)))
        'acons          (λ (x)
                          (tnil (acons x)))
        'annotate       ar-tag
        'apply          ar-apply
        'a-char         (λ (x)
                          (tnil (char? (unwrap x))))
        'a-fn           (λ (x)
                          (tnil (procedure? x)))
        'a-namespace    (λ (x)
                          (tnil (namespace? x)))
        'a-num          (λ (x)
                          (tnil (number? (unwrap x))))
        'a-socket       (λ (x)
                          (tnil (tcp-listener? x)))
        'a-str          (λ (x)
                          (tnil (string? (unwrap x))))
        'a-sym          (λ (x)
                          (tnil (symbol? (unwrap x))))
        'a-table        (λ (x)
                          (unwraps (x)
                            (tnil (or (hash? x)
                                      (symtab? x)
                                      (namespace? x)))))
        'a-tagged       (λ (x)
                          (tnil (ar-tagged? (unwrap x))))
        'a-thread       (λ (x)
                          (tnil (thread? x)))
        'an-exception   (λ (x)
                          (tnil (exn? x)))
        'an-input       (λ (x)
                          (tnil (input-port? x)))
        'an-int         (λ (x)
                          (unwraps (x)
                            (tnil (and (integer? x) (exact? x)))))
        'an-output      (λ (x)
                          (tnil (output-port? x)))
        'ar-apply       ar-apply
        'ar-assert      builtin-ar-assert
        'ar-disp        builtin-ar-disp
        'ar-iso         builtin-ar-iso
        'ar-print       builtin-ar-print
        'ar-strlen      builtin-ar-strlen
        'ar-symstr      builtin-ar-symstr
        'ar-tag-type    ar-tag-type
        'ar-uniq        builtin-ar-uniq
        'ar-write       builtin-ar-write
        'ar-writec      builtin-ar-writec
        'ar-<2          builtin-ar-<2
        'ar->2          builtin-ar->2
        'atomic-invoke  atomic-invoke
        'iround         (λ (x)
                          (inexact->exact (round (unwrap x))))
        'is2            builtin-is2
        'loc            builtin-loc
        'call-w/stdout  builtin-call-w/stdout
        'car            xcar
        'cdr            xcdr
        'charcode       (λ (x)
                          (char->integer (unwrap x)))
        'charstr        (λ (x)
                          (string (unwrap x)))
        'close          builtin-close
        'cons           xcons
        'details        (λ (c)
                          (disp-to-string (exn-message c)))
        'dir            (λ (path)
                          (ar-niltree (map path->string
                                           (directory-list (unwrap path)))))
        'err            err
        'eval-racket    builtin-eval-racket
        'file-exists    (λ (name)
                          (if (file-exists? (unwrap name)) name 'nil))
        'fnname         (λ (fn)
                          (object-name fn))
        'has            (λ (g k)
                          (tnil (has g k)))
        'hasloc         (λ (x)
                          (tnil (hasloc x)))
        'infile         infile
        'inside         get-output-string
        'inspect        inspect
        'instring       instring
        'mod            (unwrap-args modulo)
        'msec           current-milliseconds
        'namefn         (λ (name fn)
                          (procedure-rename fn (unwrap name)))
        'numstr         (unwrap-args number->string)
        'on-err         on-err
        'open-outfile   (λ (name mode exists)
                          (open-output-file (unwrap name)
                                            #:mode (unwrap mode)
                                            #:exists (unwrap exists)))
        'open-socket    (λ (num)
                          (tcp-listen (unwrap num) 50 #t))
        'outstring      open-output-string
        'protect        protect
        'quit           (unwrap-args exit)
        'sref           sref
        'racket-r-apply r-apply
        'racket-eval    eval
        'racket-list    list
        'readport       readport
        'rep            ar-rep
        'runtime        runtime
        'rootdir        rootdir
        'scar           set-mcar!
        'scdr           set-mcdr!
        'sleep          (wrapnil (unwrap-args sleep))
        'srcloc         ar-srcloc
        'stdin          current-input-port
        'stderr         current-error-port
        'stdout         current-output-port
        'strnum         (λ (s radix)
                          (unwraps (s radix)
                            (let ((r (string->number s radix)))
                              (if r r 'nil))))
        'strchars       (λ (x)
                          (ar-nillist (string->list (unwrap x))))
        'strsym         (unwrap-args string->symbol)
        'symstr         (unwrap-args symbol->string)
        'symtab         (unwrap-args new-symtab)
        'str-append     (unwrap-args string-append)
        'substr         (unwrap-args substring)
        't              't
        'table          (λ ()
                          (make-hash))
        'table-each     table-each
        'thread         thread
        'thread-wait    thread-wait
        '+              (unwrap-args +)
        '-              (unwrap-args -)
        '*              (unwrap-args *)
        '/              (unwrap-args /)))))

(create-runtime mpair  "mpair.rkt")
(create-runtime srcloc "srcloc.rkt")
