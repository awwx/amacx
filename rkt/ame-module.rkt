#lang racket

(require "boot.rkt")

(provide (rename-out (module-begin #%module-begin)))

(define-syntax module-begin
  (syntax-rules ()
    ((module-begin form ...)
     (#%plain-module-begin
       (munch 'srcloc #'(form ...))))))
