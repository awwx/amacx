#lang racket

(require racket/hash)
(require racket/runtime-path)
(require "ail-ns.rkt")
(require "data.rkt")
(require "readtables.rkt")

(provide aload caris file-each rootdir macro-expander eval-ail)

(define-runtime-path here "here")

(define rootdir (path->string (simplify-path (build-path here 'up 'up))))

(define (from-root filename)
  (build-path rootdir filename))

(define (macro-expander macro-module)
  (ref macro-module 'macro-expand))

(define (macro-expand target-module expander x)
  (expander
    (hash 'module target-module 'validate (λ (x) x))
    x))

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

(define default-ail-namespace (ail-ns))

(define (eval-ail x (ns default-ail-namespace))
  (eval (arcail x) ns))

(define (arc-eval code target-module expander)
  (define m
    (macro-expand target-module expander (ar-niltree code)))
  (eval-ail m (if (namespace? target-module)
                  target-module
                  default-ail-namespace)))

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

(define (ensure-table module tablename)
  (or (ref module tablename #f)
      (sref module tablename (make-hash))))

(define (settab module tablename k v)
  (sref (ensure-table module tablename) k v))

(define (exec2 target-module macro-module x)
  (arc-eval x target-module (macro-expander macro-module)))

(define (contains arc-list x)
  (cond ((eq? arc-list 'nil)
         #f)
        ((eq? (mcar arc-list) x)
         #t)
        (else
         (contains (mcdr arc-list) x))))

(define (has-feature module feature)
  (let ((*features* (ref module '*features* #f)))
    (and *features* (contains *features* feature))))

(define (add-feature module feature)
  (unless (has-feature module feature)
    (let ((*features* (ref module '*features* 'nil)))
      (sref module '*features* (mcons feature *features*)))))

(define (process-use target-module macro-module include-tests features)
  (for ((feature features))
    (unless (has-feature target-module feature)
      (aload feature target-module macro-module include-tests))))

(define (caris x v)
  (and (pair? x) (eq? (car x) v)))

(define (process target-module macro-module include-tests x)
  (cond ((caris x 'use)
         (process-use target-module macro-module include-tests (cdr x)))
        ((caris x 'provides)
         (add-feature target-module (cadr x)))
        (else
         (exec2 target-module macro-module x))))

(define srcdirs '("arcsrc" "arctests" "qq" "qqtests" "src" "xboot"))

(define (some test seq)
  (if (null? seq)
       #f
       (let ((r (test (car seq))))
         (or r (some test (cdr seq))))))

(define (file-exists name)
  (if (file-exists? name) name #f))

(define (findfile macro-module dirs name extension)
  (let ((r ((ref macro-module 'findfile)
            rootdir
            (ar-nillist dirs)
            (string-append name extension))))
    (if (eq? r 'nil) #f r)))

(define (findsrc macro-module name)
  (findfile macro-module srcdirs (asfilename name) ".arc"))

(define (findtest macro-module name)
  (findfile macro-module srcdirs (asfilename name) ".t"))

(define (file-each path f)
  (w/readtables
    (λ ()
      (with-input-from-file path
        (λ ()
          (let loop ()
            (let ((x (read)))
              (unless (eof-object? x)
                (f x)
                (loop)))))))))

(define (loadfile target-module macro-module include-tests src)
  (when include-tests
    (printf "> ~a~n" src))

  (file-each src (λ (x) (process target-module macro-module include-tests x))))

(define (runtest-if-exists target-module macro-module include-tests name)
  (let ((src (findtest macro-module name)))
    (when src
      (loadfile target-module macro-module include-tests src))))

(define (aload name
               target-module
               (macro-module target-module)
               (include-tests #f))
  (when (symbol? name)
    (add-feature target-module name))
  (let ((src (if (symbol? name)
                 (findsrc macro-module name)
                 name)))
    (unless src
      (error "src not found" name))
    (loadfile target-module macro-module include-tests src))
  (when include-tests
    (runtest-if-exists target-module macro-module include-tests name)))
