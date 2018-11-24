(use equals rreduce +)

(equals (rreduce +    '())        0)
(equals (rreduce +    '(1))       1)
(equals (rreduce +    '(1 2))     3)
(equals (rreduce +    '(1 2 3 4)) 10)
(equals (rreduce cons '(1 2 3 4)) '(1 2 3 . 4))
