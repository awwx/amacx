(use cxr)

(ar-assert (is2 (caar '((a)))   'a))
(ar-assert (is2 (cadr '(a b c)) 'b))
(ar-assert (is2 (car (cddr '(a b c d))) 'c))

(ar-assert (is2 (apply1 list '()) nil))
(ar-assert (is2 (car (apply1 list '(1))) 1))
(ar-assert (is2 (cdr (apply1 list '(1))) nil))
(ar-assert (is2 (car (apply1 list '(1 2))) 1))
(ar-assert (is2 (cadr (apply1 list '(1 2))) 2))
(ar-assert (is2 (cddr (apply1 list '(1 2))) nil))
