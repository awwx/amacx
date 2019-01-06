(use equals eval)

(equals (eval '(+ 1 2) this-container (gen-compiler ac-rules))
        3)
