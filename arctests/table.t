(use arcbase equals obj ssyntax true false)

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
