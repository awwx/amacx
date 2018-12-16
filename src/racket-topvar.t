(use arcbase racket-topvar equals eval load)

(let container (racket-topvar-container)
  (equals (eval '(+ 1 2) container)
          3)

  (use-feature container 'reduce)

  (equals (eval '(reduce + '(1 2 3 4)) container)
          10)

  (load "src/len.t" container))
