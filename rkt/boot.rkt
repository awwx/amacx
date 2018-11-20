#lang racket

(require racket/runtime-path)

(require "ar.rkt")
(require "eval-ail.rkt")
(require "symtab.rkt")

(define-runtime-path here "here")

(define (from-here filename)
  (simplify-path (build-path here 'up filename)))

(define expanded-boot-path (from-here "../xcompile/boot.expanded"))

(define (px msg x)
  (printf "~a: ~s~n" msg x)
  x)

;; Phase one

(define (caris x v)
  (and (pair? x) (eq? (car x) v)))

(define module1 (builtins))

(define (inject x)
  (cond ((box? x)
         (cond ((eq? (unbox x) '*module*)
                module1)
               (else
                (hash-ref module1 (unbox x)))))

        ((pair? x)
         (cons (inject (car x))
               (inject (cdr x))))

        (else
         x)))

(define (demunch x)
  (cond ((caris x 'quote-xVrP8JItk2Ot)
         `(quote-xVrP8JItk2Ot ,(ar-niltree (inject (cadr x)))))

        ((pair? x)
         (cons (demunch (car x))
               (demunch (cdr x))))

        (else x)))

(define (mcaris x v)
  (and (mpair? x) (eq? (mcar x) v)))

(define (mcadr x)
  (mcar (mcdr x)))

; This is like ar-denil except that quoted values are left unconverted.

(define (arcail x)
  (cond ((mcaris x 'quote-xVrP8JItk2Ot)
         (list 'quote-xVrP8JItk2Ot (mcadr x)))

        ((mpair? x)
         (cons (arcail-car (mcar x))
               (arcail-cdr (mcdr x))))

        (else x)))

(define (arcail-car x)
  (if (eq? x 'nil)
      'nil
      (arcail x)))

(define (arcail-cdr x)
  (if (eq? x 'nil)
      '()
      (arcail x)))

;(print-hash-table #f)

(define (exec1 x)
  (let ((a (demunch x)))
    (eval-ail a))
  (void))

(define (file-each path f)
  (with-input-from-file path
    (λ ()
      (let loop ()
        (let ((x (read)))
          (unless (eof-object? x)
            (f x)
            (loop)))))))

(file-each expanded-boot-path exec1)

(define (macro-expand1 module x)
  ((hash-ref module1 'macro-expand)
   (hash 'module module 'validate (λ (x) x))
   x))

(define (arc-eval1 module code)
  (define m
    (macro-expand1 module (ar-niltree code)))
  (define ail (arcail m))
  (eval-ail ail))


;; Phase two

(define convert-filename-chars
  (hash #\/ "slash"
        #\\ "backslash"
        #\_ "underline"))

(define (tostr s)
  (cond ((string? s)
         s)
        ((char? s)
         (string s))
        ((symbol? s)
         (symbol->string s))
        (else
         (error "can't convert to string" s))))

(define (str . args)
  (apply string-append (map tostr args)))

(module+ test (require rackunit/chk)
  (chk (str "a" #\b 'c "d") "abcd"))

(define (asfilename s)
  (apply str
    (map (λ (c)
           (cond ((hash-has-key? convert-filename-chars c)
                  (str "_" (hash-ref convert-filename-chars c) "_"))
                 (else
                  (string c))))
         (string->list (str s)))))

(define include-tests #t)

(when include-tests
  (printf "------ phase two~n"))

(define (replace-tree mapping x)
  (cond ((and (symbol? x)
              (hash-has-key? mapping x))
         (hash-ref mapping x))
        ((pair? x)
         (cons (replace-tree mapping (car x))
               (replace-tree mapping (cdr x))))
        (else x)))

(define $renames
  (hash '$quote   'quote-xVrP8JItk2Ot
        '$fn      'fn-xVrP8JItk2Ot
        '$assign  'assign-xVrP8JItk2Ot
        '$if      'if-xVrP8JItk2Ot
        '$call    'call-xVrP8JItk2Ot))

(define (rename$ x)
  (replace-tree $renames x))

(define module2 (new-symtab (builtins)))

((λ ()
  (sref module2 '*loaded* (make-hash))
  (sref module2 '*provisional* (make-hash))
  (void)))

(define (exec2 x)
  (arc-eval1 module2 (rename$ x)))

(define (loaded module sym)
  (let ((*loaded* (ref module '*loaded* 'nil)))
    (and (ar-true? *loaded*)
         (ar-true? (ref *loaded* sym 'nil)))))

(define (process-use module features)
  (for ((feature features))
    (unless (or (and (not (ar-true? (ref (ref module '*provisional*) feature)))
                     (ar-true? (ref module feature 'nil)))
                (loaded module feature))
      (load2 module feature))))

(define (process module x)
  (cond ((caris x 'use)
         (process-use module (cdr x)))
        ((caris x 'provisional)
         (sref (ref module '*provisional*) (cadr x) 't))
        ((caris x 'provides)
         (sref (ref module '*loaded*) (cadr x) 't))
        (else
         (exec2 x))))

(define srcdirs '("../qq" "../src"))

(define testdirs '("../tests"))

(define (some test seq)
  (if (null? seq)
       #f
       (let ((r (test (car seq))))
         (or r (some test (cdr seq))))))

(define (file-exists name)
  (if (file-exists? name) name #f))

(define (findfile dirs name extension)
  (some (λ (dir)
          (file-exists
            (from-here (string-append dir "/" name extension))))
        dirs))

(define (findsrc name)
  (findfile srcdirs (asfilename name) ".arc"))

(define (findtest name)
  (findfile testdirs (asfilename name) ".t"))

(define (loadfile module src)
  (display src)
  (newline)
  (file-each src (λ (x) (process module x))))

(define (runtest-if-exists module name)
  (let ((src (findtest name)))
    (when src
      (loadfile module src))))

(define (load2 module name)
  (let ((*loaded* (ref module '*loaded* #f)))
    (when (and (symbol? name) *loaded*)
      (sref *loaded* name 't)))
  (let ((src (if (symbol? name)
                 (findsrc name)
                 (from-here name))))
    (unless src
      (error "not found" name))
    (loadfile module src))
  (let ((*provisional* (ref module '*provisional* #f)))
    (when (and *provisional* (symbol? name))
      (sref *provisional* name 'nil)))
  (when include-tests
    (runtest-if-exists module name)))

(load2 module2 'macro)

(define (macro-expand2 module x)
  ((symtab-ref module2 'macro-expand)
   (hash 'module module 'validate (λ (x) x))
   x))

; (define (arc-eval2 module code)
;   (eval-ail
;     (arcail
;       (macro-expand2 module (ar-niltree code)))))

; (define (aload module name)
;   (file-each name
;     (λ (x)
;       (arc-eval2 module x))))

; (when include-tests
;   (aload module2 "perftest.arc"))

(printf "all done\n")
