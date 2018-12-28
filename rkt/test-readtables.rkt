#lang reader "amacx.rkt"

; Test readtables using the "amacx" Racket language.

(use arcbase equals)

(equals § "Hello\nand greetings.")
  │ Hello
  │ and greetings.

(equals '[a b c] '(square-bracket a b c))

(equals '{a 1, b 2} '(curly-bracket (a 1) (b 2)))
