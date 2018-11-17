; Quasiquote expressions not supported in Arc 3.1

(test `(a ,@'b)
  '(a . b))

(test ``,,3
  3)

(test ``,,'a
  'a)

; todo
; don't have eval yet
; (test (eval ``(,@'(,@'(a b c))) '(a b c)))
