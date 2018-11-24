#lang racket

(require racket/hash)
(require racket/runtime-path)

(require "ar.rkt")
(require "eval-ail.rkt")
(require "symtab.rkt")

(provide phase1 phase2 aload)

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

(define (inject module x)
  (cond ((box? x)
         (cond ((eq? (unbox x) '*module*)
                module)
               (else
                (hash-ref module (unbox x)))))

        ((pair? x)
         (cons (inject module (car x))
               (inject module (cdr x))))

        (else
         x)))

(define (demunch module x)
  (cond ((caris x 'quote-xVrP8JItk2Ot)
         `(quote-xVrP8JItk2Ot ,(ar-niltree (inject module (cadr x)))))

        ((pair? x)
         (cons (demunch module (car x))
               (demunch module (cdr x))))

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

(define (exec1 module x)
  (let ((a (demunch module x)))
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

(define (phase1 (include-tests #f))
  (when include-tests
    (printf "------ phase one~n"))
  (let ((module (make-hash)))
    (hash-union! module builtins)
    (file-each expanded-boot-path (λ (x) (exec1 module x)))
    module))

(define (macro-expander macro-module)
  (ref macro-module 'macro-expand))

(define (macro-expand target-module expander x)
  (expander
    (hash 'module target-module 'validate (λ (x) x))
    x))

(define (arc-eval code target-module expander)
  (define m
    (macro-expand target-module expander (ar-niltree code)))
  (define ail (arcail m))
  (eval-ail ail))


;; Phase two

(define convert-filename-chars
  (hash #\/ "slash"
        #\\ "backslash"
        #\_ "underline"
        #\< "lt"
        #\> "gt"))

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

(define (ensure-table module tablename)
  (or (ref module tablename #f)
      (sref module tablename (make-hash))))

(define (settab module tablename k v)
  (sref (ensure-table module tablename) k v))

(define (exec2 target-module expander x)
  (arc-eval (rename$ x) target-module expander))

(define (loaded module sym)
  (let ((*loaded* (ref module '*loaded* 'nil)))
    (and (ar-true? *loaded*)
         (ar-true? (ref *loaded* sym 'nil)))))

(define (provisional? target-module feature)
  (let ((*provisional* (ref target-module '*provisional* #f)))
    (and *provisional* (ar-true? (ref *provisional* feature)))))

(define (process-use target-module expander include-tests features)
  (for ((feature features))
    (unless (or (and (not (provisional? target-module feature))
                     (ar-true? (ref target-module feature 'nil)))
                (loaded target-module feature))
      (aload feature target-module expander include-tests))))

(define (process target-module expander include-tests x)
  (cond ((caris x 'use)
         (process-use target-module expander include-tests (cdr x)))
        ((caris x 'provisional)
         (settab target-module '*provisional* (cadr x) 't))
        ((caris x 'provides)
         (settab target-module '*loaded* (cadr x) 't))
        (else
         (exec2 target-module expander x))))

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

(define (loadfile target-module expander include-tests src)
  (when include-tests
    (display src)
    (newline))
  (file-each src (λ (x) (process target-module expander include-tests x))))

(define (runtest-if-exists expander target-module include-tests name)
  (let ((src (findtest name)))
    (when src
      (loadfile target-module expander include-tests src))))

(define (aload name
               target-module
               (expander (macro-expander target-module))
               (include-tests #f))
  (when (symbol? name)
    (settab target-module '*loaded* name 't))
  (let ((src (if (symbol? name)
                 (findsrc name)
                 name)))
    (unless src
      (error "not found" name))
    (loadfile target-module expander include-tests src))
  (when (symbol? name)
    (settab target-module '*provisional* name 'nil))
  (when include-tests
    (runtest-if-exists expander target-module include-tests name)))

(define (phase2 include-tests (module1 (phase1 include-tests)))
  (when include-tests
    (printf "------ phase two~n"))
  (define module2 (new-symtab builtins))
  (aload 'macro module2 (macro-expander module1) include-tests)
  (when include-tests
    (printf "tests done\n"))
  module2)
