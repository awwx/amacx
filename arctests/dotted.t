(use equals dotted)

(equals (dotted 1)          nil)
(equals (dotted '(1 . 2))   t)
(equals (dotted '(1 2))     nil)
(equals (dotted '(1 2 . 3)) t)
(equals (dotted '(1 2 3))   nil)
