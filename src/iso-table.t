(use equals iso-table obj true false)

(true (table-equal (obj) (obj)))
(true (table-equal (obj a 1) (obj a 1)))
(true (table-equal (obj a 1 b 2) (obj a 1 b 2)))

(true  (iso 'a 'a))
(false (iso 'a 'b))

(true  (iso '(a b) '(a b)))
(false (iso '(a b) '(a 9)))

(true  (iso (obj a 1) (obj a 1)))
(false (iso (obj a 1) (obj a 2)))
(false (iso (obj a 1) (obj b 1)))

(true  (iso (obj a 1 b 2) (obj a 1 b 2)))
(false (iso (obj a 1 b 2) (obj a 1 b 9)))
(false (iso (obj a 1 b 2) (obj a 1 c 2)))

(true  (iso (obj (a b) 1) (obj (a b) 1)))
(false (iso (obj (a b) 1) (obj (a c) 1)))
(false (iso (obj (a b) 1) (obj (a b) 2)))
