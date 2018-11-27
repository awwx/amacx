(use equals quasiquote)

(equals (proper '())        t)
(equals (proper 'a)         nil)
(equals (proper '(a))       t)
(equals (proper '(a . b))   nil)
(equals (proper '(a b))     t)
(equals (proper '(a b . c)) nil)
(equals (proper '(a b c))   t)
