(use true with is)

(with ()
  (true t))

(with (a 1)
  (true (is a 1)))

(with (a 1 b 2)
  (true (is a 1))
  (true (is b 2)))
