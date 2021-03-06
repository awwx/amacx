(use topvar)

(ar-assert (topvar t))
(ar-assert (is2 ((topvar +) 1 2) 3))
(ar-assert (is2 ((topvar ar-<2) 1 2) t))
(ar-assert (is2 ((topvar ar-<2) 2 2) nil))
(ar-assert (is2 ((topvar ar-<2) 3 2) nil))

(ar-assert (is2 (($fn args args)) nil))

(ar-assert (is2 (car (($fn args args) 1)) 1))
(ar-assert (is2 (cdr (($fn args args) 1)) nil))

(ar-assert (is2 (car      (($fn args args) 1 2))  1))
(ar-assert (is2 (car (cdr (($fn args args) 1 2))) 2))
(ar-assert (is2 (cdr (cdr (($fn args args) 1 2))) nil))

(ar-assert (is2 (($fn (a . rest) a)          1 2) 1))
(ar-assert (is2 (($fn (a . rest) (car rest)) 1 2) 2))
(ar-assert (is2 (($fn (a . rest) (cdr rest)) 1 2) nil))
