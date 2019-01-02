(use arc equals)

(equals (last '())      nil)
(equals (last '(a))     'a)
(equals (last '(a b))   'b)
(equals (last '(a b c)) 'c)
