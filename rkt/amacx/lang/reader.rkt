#lang s-exp syntax/module-reader
#:wrapper2 wrapper2
#:language amacx-module-path

; TODO what, if anything, should we provide for language info
; that's different from Racket?
#:language-info '#(racket/language-info get-info #f)

(require racket/runtime-path)
(require "../../readtables.rkt")

(define-runtime-path amacx-module-path "../../amacx-module.rkt")

(define (wrapper2 input-port proceed stx?)
  (w/readtables input-port
    (λ (splicing-port)
      (proceed splicing-port))))
