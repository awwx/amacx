#lang racket

(require "ar.rkt")
(require "boot.rkt")

(define args (current-command-line-arguments))

(when (> (vector-length args) 0)
  (define file (vector-ref args 0))
  (define module (phase2))
  (sref module 'aload aload)
  (aload module file)
  (void))
