; racket -f expand.scm

(require "ac.scm")
(require "brackets.scm")
(use-bracket-readtable)

(require "../rkt/uniq.rkt")

(define _ar-uniq ar-uniq)

(define _eq? eq?)

(define _box box)

(define (_pprint x)
  (pretty-print (ac-denil x)))

(define (_has g key)
  (tnil (hash-has-key? g key)))

(define (_namefn name fn)
  (procedure-rename fn name))

(define (_fnname fn)
  (object-name fn))

(xdef a-tagged
  (lambda (x)
    (tnil (ar-tagged? x))))

(xdef ac-denil ac-denil)

(aload "arc.arc")
(aload "expand.arc")
