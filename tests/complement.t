(use equals complement <)

(equals ((complement car) nil)  t)
(equals ((complement car) '(a)) nil)
(equals (< 3 7) t)
(equals ((complement <) 3 7) nil)
