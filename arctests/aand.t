(use arc equals)

(equals (aand)                     t)
(equals (aand nil)                 nil)
(equals (aand 1)                   1)
(equals (aand nil it)              nil)
(equals (aand 1 it)                1)
(equals (aand 1 (+ it 1) (+ it 1)) 3)
