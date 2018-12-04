(use arcbase equals quasiquote)

; These are quasiquote expressions supported in Arc 3.1

(equals `40
  40)

(equals `(a (b . c) d)
  '(a (b . c) d))

(equals `(a ,(+ 2 3) b)
  '(a 5 b))

(equals `(a ,@(list 'b 'c) d)
  '(a b c d))

(equals `(,@ '(a b c))
  '(a b c))

(equals `(a (b . c) d)
  '(a (b . c) d))

(equals `(a ,(+ 2 3) b)
  '(a 5 b))

(equals `(a ,@(list 'b 'c) d)
  '(a b c d))


; Quasiquote expressions not supported in Arc 3.1

(equals `(a ,@'b)
  '(a . b))

(equals ``,,3
  3)

(equals ``,,'a
  'a)

; todo
; don't have eval yet
; (equals (eval ``(,@'(,@'(a b c))) '(a b c)))
