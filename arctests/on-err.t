(use arcbase equals)

(equals
  (on-err
    (fn (c)
      (details c))
    (fn ()
      (err "foo")))
  "foo")

(equals
  (on-err
    (fn (c1)
      (details c1))
    (fn ()
      (on-err
        (fn (c2)
          (err "two"))
        (fn ()
          (err "one")))))
  "two")
