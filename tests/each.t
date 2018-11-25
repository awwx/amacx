(use each complex-fn prn case obj equals)

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
