(use arc equals)

(equals (adjoin 'a '())    '(a))
(equals (adjoin 'a '(a))   '(a))
(equals (adjoin 'a '(b))   '(a b))
(equals (adjoin 'a '(a b)) '(a b))
(equals (adjoin 'a '(b a)) '(b a))
(equals (adjoin 'a '(b c)) '(a b c))
