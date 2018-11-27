(use equals quasiquote)

(equals (dotted-list 1)     1)
(equals (dotted-list 1 2)   '(1 . 2))
(equals (dotted-list 1 2 3) '(1 2 . 3))
