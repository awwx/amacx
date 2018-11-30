(use equals +str)

(equals (+ "abc" 123 'xyz)
        "abc123xyz")

(equals (+ #\a #\b #\c)
        "abc")
