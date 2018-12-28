(use arcboot pair)

((fn (x)
  (ar-assert (is2 (car  (car x))  'a))
  (ar-assert (is2 (cadr (car x))  1))
  (ar-assert (is2 (car  (cadr x)) 'b))
  (ar-assert (is2 (cadr (cadr x)) 2)))
 (pair '(a 1 b 2)))
