(use simple-equals apply)

(equals (apply +)     0)
(equals (apply + '()) 0)

(equals (apply + 1 '()) 1)
(equals (apply + '(1))  1)

(equals (apply + 1 2 '()) 3)
(equals (apply + 1 '(2))  3)
(equals (apply + '(1 2))  3)

(equals (apply + '(1 2 3)) 6)

(equals (apply + '(1 2 3 4))  10)
(equals (apply + 1 '(2 3 4))  10)
(equals (apply + 1 2 '(3 4))  10)
(equals (apply + 1 2 3 '(4))  10)
(equals (apply + 1 2 3 4 '()) 10)
