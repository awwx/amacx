(use each complex-fn case obj equals accum)

(equals (accum a
          (each x '(1 2 3 4)
            (a x)))
        '(1 2 3 4))

(with (a nil b nil c nil)
  (each (k v) (obj a 1 b 2 c 3)
    (case k
      a (assign a v)
      b (assign b v)
      c (assign c v)
        (err "oops" k)))
  (equals a 1)
  (equals b 2)
  (equals c 3))

(equals (accum a
          (each c "abcd"
            (a c)))
        '(#\a #\b #\c #\d))
