(use cxr)

(ar-assert (is2 (caar '((a)))   'a))
(ar-assert (is2 (cadr '(a b c)) 'b))
(ar-assert (is2 (car (cddr '(a b c d))) 'c))

(ar-assert (is2 (ar-apply list '()) nil))
(ar-assert (is2 (car (ar-apply list '(1))) 1))
(ar-assert (is2 (cdr (ar-apply list '(1))) nil))
(ar-assert (is2 (car (ar-apply list '(1 2))) 1))
(ar-assert (is2 (cadr (ar-apply list '(1 2))) 2))
(ar-assert (is2 (cddr (ar-apply list '(1 2))) nil))
