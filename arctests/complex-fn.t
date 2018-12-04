(use arcbase equals complex-fn)

(equals (fn-complex-args? '())      nil)
(equals (fn-complex-args? 'a)       nil)
(equals (fn-complex-args? '(a))     nil)
(equals (fn-complex-args? '(a . b)) nil)
(equals (fn-complex-args? '((a)))   t)

; todo not a runtime test
; (equals (let fn + (fn 6 7)) 13)

(equals ((fn ((o a 3)) a))
        3)

(equals ((fn ((o a 3)) a) 4)
        4)

(equals ((fn ((a b c))
           (list a b c))
         '(1 2 3))
        '(1 2 3))

(equals ((fn ((a (b c) d))
           (list a b c d))
         '(1 (2 3) 4))
        '(1 2 3 4))
