(use arcbase equals accum repeat ++)

(equals
  (accum a
    (let n 0
      (repeat 3 (do (++ n)
                    (a n)))))
  '(1 2 3))
