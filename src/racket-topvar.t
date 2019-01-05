(use arcbase racket-topvar equals eval load)

(equals (ail-topvar-ac (obj) '($topvar--xVrP8JItk2Ot foo))
        '($topvar--xVrP8JItk2Ot foo))

(let container (racket-topvar-container)
  (equals (eval '(+ 1 2) container)
          3)

  (use-features container '(reduce))

  (equals (eval '(reduce + '(1 2 3 4)) container)
          10)

  (load "arctests/len.t" container))
