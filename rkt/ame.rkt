(module ame syntax/module-reader
  #:wrapper2 wrapper2
  #:language ame-module-path

  (require racket/runtime-path)
  (require "readtables.rkt")

  (define-runtime-path ame-module-path "ame-module.rkt")

  (define (wrapper2 input-port proceed stx?)
    (w/readtables
      (λ ()
        (proceed input-port)))))
