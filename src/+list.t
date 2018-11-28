(use equals +list)

(equals (+)
        0)

(equals (+ 1 2 3 4)
        10)

(equals (+ '())
        '())

(equals (+ '() '(a) '(b))
        '(a b))

(equals (+ '(a b) '(c d e))
        '(a b c d e))
