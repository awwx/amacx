(use arcbase equals literal)

(equals (literal 3)              t)
(equals (literal car)            t)
(equals (literal "abc")          t)
(equals (literal 'foo)           nil)
(equals (literal 'nil)           t)
(equals (literal '(4 5))         nil)
(equals (literal '(quote 3))     t)
(equals (literal (list quote 3)) t)
