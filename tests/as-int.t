(use equals as-int)

(equals (as-int 42)           42)
(equals (as-int 1.23)          1)
(equals (as-int 1.9)           2)
(equals (as-int "123")       123)
(equals (as-int "7b" 16)     123)
(equals (as-int #\A)          65)
(equals (as-int #\λ)      #x03BB)
