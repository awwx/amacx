(use arc equals iso-table)

; TODO mutable-list-assumption

(let a '(1 2 3)
  (equals pop.a 1)
  (equals a '(2 3))

  (equals pop.a 2)
  (equals a '(3))

  (equals pop.a 3)
  (equals a '()))

(let a (obj x 'foo y '(1 2 3 4) z 'bar)
  (equals (pop a!y) 1)
  (equals a (obj x 'foo y '(2 3 4) z 'bar)))
