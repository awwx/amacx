(use equals len)

(equals (len '()) 0)
(equals (len '(a)) 1)
(equals (len '(a b)) 2)
(equals (len '(a b c)) 3)

(equals (len "")    0)
(equals (len "abc") 3)