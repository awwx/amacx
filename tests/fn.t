; (equals (fn-complex-opt 'f 'list '(cdr g1))
;   '((f (if (acons (cdr g1)) (car (cdr g1)) list))))

(equals (cadar  '((a b c) d e)) 'b)
(equals (caddar '((a b c) d e)) 'c)

; (equals (fn-complex-args 'xs '(car gs1))
;   '((xs (car gs1))))

; (equals (fn-complex-args '(xs (o f list)) 'gs1)
;   `((xs (,car gs1))
;     (f  (if (acons (,cdr gs1))
;              (car (,cdr gs1))
;              list))))

(equals (fn-complex-args? '())      nil)
(equals (fn-complex-args? 'a)       nil)
(equals (fn-complex-args? '(a))     nil)
(equals (fn-complex-args? '(a . b)) nil)
(equals (fn-complex-args? '((a)))   t)

; todo not a runtime test
; (equals (let fn + (fn 6 7)) 13)

(equals ((fn ((o a 3)) a))
        3)

(equals ((fn ((a b c))
           (list a b c))
         '(1 2 3))
        '(1 2 3))

(equals ((fn ((a (b c) d))
           (list a b c d))
         '(1 (2 3) 4))
        '(1 2 3 4))
