(use equals accum atomic)

(equals
  (accum a
    (let t1 (thread (fn ()
                      (atomic
                        (sleep .3)
                        (a 'one))))
    (sleep .1)
    (let t2 (thread (fn ()
                      (atomic
                        (sleep .1)
                        (a 'two))))
      (thread-wait t1)
      (thread-wait t2))))
  '(one two))
