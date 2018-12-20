(use arcbase write tostring simple-equals do1)

(equals (tostring (write 123))
        "123")

(equals (tostring (write 'foo))
        "foo")

(equals (tostring (write 'nil))
        "nil")

(equals (tostring (write "foo"))
        "\"foo\"")

(equals (tostring (write '(a b c)))
        "(a b c)")

(equals (tostring (write #\A))
        "#\\A")

(equals (do1 (write 'yes)
             (ar-disp "\n" (stdout)))
        nil)
