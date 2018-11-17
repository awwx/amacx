(ar-assert (module-var t))
(ar-assert (ar-is2 ((module-var +) 1 2) 3))
(ar-assert (ar-is2 ((module-var ar-<2) 1 2) t))
(ar-assert (ar-is2 ((module-var ar-<2) 2 2) nil))
(ar-assert (ar-is2 ((module-var ar-<2) 3 2) nil))

(ar-assert (ar-is2 (($fn args args)) nil))

(ar-assert (ar-is2 (car (($fn args args) 1)) 1))
(ar-assert (ar-is2 (cdr (($fn args args) 1)) nil))

(ar-assert (ar-is2 (car      (($fn args args) 1 2))  1))
(ar-assert (ar-is2 (car (cdr (($fn args args) 1 2))) 2))
(ar-assert (ar-is2 (cdr (cdr (($fn args args) 1 2))) nil))

(ar-assert (ar-is2 (($fn (a . rest) a)          1 2) 1))
(ar-assert (ar-is2 (($fn (a . rest) (car rest)) 1 2) 2))
(ar-assert (ar-is2 (($fn (a . rest) (cdr rest)) 1 2) nil))
