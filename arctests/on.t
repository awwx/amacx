(use arc equals)

(equals
  (accum a
    (on x '(a b c d)
      (a (list index x))))
  '((0 a) (1 b) (2 c) (3 d)))
