(ar-assert (is2 ((fn () 651))         651))
(ar-assert (is2 ((fn () 400 401 402)) 402))

(ar-assert (is2 ((fn (a) a) 33) 33))

(ar-assert (is2 ((fn (a b) a) 33 44) 33))
(ar-assert (is2 ((fn (a b) b) 33 44) 44))

(ar-assert (is2 (((fn (a)
                    (fn ()
                      a))
                  42))
                42))

(ar-assert (is2 (car           ((fn args args) 1 2 3))   1))
(ar-assert (is2 (car (cdr      ((fn args args) 1 2 3)))  2))
(ar-assert (is2 (car (cdr (cdr ((fn args args) 1 2 3)))) 3))

(ar-assert (is2 (fnname (namefn 'foo (fn () 0)))
                'foo))
