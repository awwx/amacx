; racket -f expand.scm

(require "ac.scm")
(require "brackets.scm")
(use-bracket-readtable)

(require "../rkt/uniq.rkt")
(require "../rkt/symtab.rkt")

;(print-hash-table #f)

(define _ar-uniq ar-uniq)

(define _eq? eq?)

(define _box box)

(define (_pprint x)
  (pretty-print (ac-denil x)))

(define (_has g key)
  (tnil (cond ((symtab? g)
               (symtab-has-key? g key))
              (else
               (hash-has-key? g key)))))

(define _symtab new-symtab)

(define ac-sref _sref)

(define (_sref g v k)
  (cond ((symtab? g)
         (symtab-set! g k v))
        (else
         (ac-sref g v k))))

(define ac-maptable _maptable)

(define (_maptable fn table)
  (cond ((symtab? table)
         (symtab-each table fn))
        (else
         (ac-maptable fn table))))

(define ac-type _type)

(define (_type x)
  (cond ((symtab? x)
         'table)
        (else
         (ac-type x))))

(define (_namefn name fn)
  (procedure-rename fn name))

(define (_fnname fn)
  (object-name fn))

(xdef a-tagged
  (lambda (x)
    (tnil (ar-tagged? x))))

(xdef ac-denil ac-denil)

(define argv (current-command-line-arguments))

(require racket/runtime-path)
(define-runtime-path here "here")
(define _rootdir (path->string (simplify-path (build-path here 'up 'up))))

(aload "arc.arc")
(aload "expand.arc")
