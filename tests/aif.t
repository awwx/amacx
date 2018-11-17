(equals (aif 1 it)               1)
(equals (aif nil it 3)           3)
(equals (aif 1 it 3)             1)
(equals (aif 1 it 2 it)          1)
(equals (aif nil it 2 it)        2)
(equals (aif nil it nil it 4 it) 4)

(let x 0
  (equals (aif (do (assign x (+ x 1))
                   x)
               it)
          1))
