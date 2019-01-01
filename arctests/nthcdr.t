(use arc equals)

(equals (nthcdr 0 '(a b c d)) '(a b c d))
(equals (nthcdr 2 '(a b c d)) '(c d))
(equals (nthcdr 4 '(a b c d)) '())
(equals (nthcdr 5 '(a b c d)) '())
