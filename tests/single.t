(equals (single '())      nil)
(equals (single '(a))     t)
(equals (single '(a . b)) nil)
(equals (single '(a b))   nil)
