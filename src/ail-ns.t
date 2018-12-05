(use arcbase equals)

(let g (ail-namespace)
  (equals (a-table g) 't)
  (sref g 'a 1)
  (sref g 'b 2)
  (equals (g 'a) 1)
  (equals (g 'b) 2)
  (equals (racket-eval 'a g) 1))
