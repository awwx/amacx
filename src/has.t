(use arcbase true false obj)

(true  (has (obj a 1) 'a))
(false (has (obj a 1) 'b))

(true  (has (obj (a b) 1) '(a b)))
(false (has (obj (a b) 1) '(a x)))
