(use arcbase len obj equals)

(equals (len (obj))             0)
(equals (len (obj a 1))         1)
(equals (len (obj a 1 b 2))     2)
(equals (len (obj a 1 b 2 c 3)) 3)
