(use arcboot equals)

; (equals ((fn ())) nil)

(equals ((fn (a b) a) 33 44) 33)
(equals ((fn (a b) b) 33 44) 44)

(equals (((fn (a)
            (fn ()
              a))
          42))
        42)

(equals (car           ((fn args args) 1 2 3))   1)
(equals (car (cdr      ((fn args args) 1 2 3)))  2)
(equals (car (cdr (cdr ((fn args args) 1 2 3)))) 3)

(equals ("abcd" 2) #\c)

(equals (apply "abcd" '(2)) #\c)
