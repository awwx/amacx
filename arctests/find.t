(use equals find)

(equals (find 3      '(1 2 3 4)) 3)
(equals (find 5      '(1 2 3 4)) nil)
(equals (find an-int '(a b 2 c)) 2)
(equals (find #\b    "abcd")     #\b)
