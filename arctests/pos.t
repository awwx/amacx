(use arc equals)

(equals (pos #\x "abcdef")   nil)
(equals (pos #\c "abcdef")   2)
(equals (pos #\c "abcdef" 2) 2)
(equals (pos #\c "abcdef" 3) nil)

; TODO pos with lists
