#lang racket

(require racket/hash)
(require racket/runtime-path)
(require "ail-ns.rkt")
(require "data.rkt")

(provide aload rootdir eval-ail)

(define-runtime-path here "here")

(define rootdir (path->string (simplify-path (build-path here 'up 'up))))

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

(define (findsrc macro-module name)
  (let ((r ((ref macro-module 'findsrc)
            macro-module
            name)))
    (if (eq? r 'nil) #f r)))

(define (findtest macro-module name)
  (let ((r ((ref macro-module 'findtest)
            macro-module
            name)))
    (if (eq? r 'nil) #f r)))

(define (inline-tests target-module)
  (ar-true? (ref target-module '*inline-tests* 'nil)))

(define (loadfile target-module macro-module src)
  ((or (ref target-module 'loadfile #f)
       (ref macro-module 'loadfile))
   target-module
   src))

(define (runtest-if-exists target-module macro-module name)
  (let ((src (findtest macro-module name)))
    (when src
      (loadfile target-module macro-module src))))

(define (aload name
               target-module
               (macro-module target-module))
  (when (symbol? name)
    (add-feature target-module name))

  (let ((src (if (symbol? name)
                 (findsrc macro-module name)
                 name)))
    (unless src
      (error "src not found" name))
    (loadfile target-module macro-module src))

  (when (inline-tests target-module)
    (runtest-if-exists target-module macro-module name)))
