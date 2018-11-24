(use equals rfn if)

(equals ((rfn foo (i x)
              (if (is i 0)
                  x
                  (foo (- i 1) (* i x))))
         3 5)
        30)
