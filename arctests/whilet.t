(use arcbase whilet equals accum > ++)

(equals
  (accum a
    (let x 5
      (whilet y (and (> x 0) x)
        (a y)
        (-- x))))
  '(5 4 3 2 1))
