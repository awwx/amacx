(use arcbase equals literal-value)

(equals (literal-value 3)              3)
(equals (literal-value car)            car)
(equals (literal-value "abc")          "abc")
(equals (literal-value 'nil)           nil)
(equals (literal-value '(quote 3))     3)
(equals (literal-value (list quote 3)) 3)
