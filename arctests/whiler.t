(use arcboot equals accum let whiler ret)

(equals
  (accum a
    (let n '(a b c)
      (whiler x (ret y (car n) (assign n (cdr n))) nil
        (a x))))
  '(a b c))
