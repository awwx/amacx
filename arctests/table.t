(use arc equals ssyntax true false sets-iso)

(let g (obj a 1 b 2)
  (true (has g 'a))
  (false (has g 'x))
  (equals g!a 1)
  (equals g!b 2)
  (equals (g 'x 9) 9))

(let g (obj (a b) 1)
  (true (has g '(a b)))
  (false (has g '(a x)))
  (equals (g '(a b)) 1)
  (equals (g '(a x) 9) 9))

(let g (obj a 1 b 2 c 3)
  (true (sets-iso (keys g) '(a b c)))

  ; TODO Arc 3.1
  (sref g 'b nil)
  (true (sets-iso (keys g) '(a c)))

  (sref g 'a nil)
  (true (sets-iso (keys g) '(c)))

  (sref g 'c nil)
  (true (sets-iso (keys g) '())))
