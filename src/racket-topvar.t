(use arcbase racket-topvar equals eval load)

(let container (racket-topvar-container)
  (equals (eval '(+ 1 2) container)
          3)
  (load "src/len.t" container))
