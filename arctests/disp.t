(use arcbase disp tostring simple-equals)

(equals (tostring (disp 123))
        "123")

(equals (tostring (disp 'foo))
        "foo")

(equals (tostring (disp 'nil))
        "nil")

(equals (tostring (disp "foo"))
        "foo")

(equals (tostring (disp '(a b c)))
        "(a b c)")

(equals (tostring (disp #\A))
        "A")
