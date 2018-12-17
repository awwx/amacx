(use arcbase catcherr simple-equals)

(equals (catcherr (/ 1 0))
        '(err "/: division by zero"))
