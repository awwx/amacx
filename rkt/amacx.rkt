(module amacx syntax/module-reader
  #:wrapper2 wrapper2
  #:language amacx-module-path

  (require racket/runtime-path)
  (require "readtables.rkt")

  (define-runtime-path amacx-module-path "amacx-module.rkt")

  (define (wrapper2 input-port proceed stx?)
    (w/readtables input-port
      (λ (splicing-port)
        (proceed splicing-port)))))
