(use true false sets-iso)

(true  (sets-iso '() '()))

(true  (sets-iso '(a) '(a)))
(false (sets-iso '(a) '(b)))

(true  (sets-iso '(a b) '(a b)))
(true  (sets-iso '(b a) '(b a)))
(false (sets-iso '(a b) '(a a)))
(false (sets-iso '(a b) '(c a)))

(true  (sets-iso '((a))   '((a))))
(true  (sets-iso '((a b)) '((a b))))
(false (sets-iso '((a b)) '((b a))))
