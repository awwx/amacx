(use simple-equals)

(equals (ar-strlen "abc") 3)

(equals (ar-strlen (ar-symstr (ar-uniq nil nil)))
        16)
