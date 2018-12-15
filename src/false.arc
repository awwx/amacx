(use mac true no quasiquote)

(mac false (x)
  `(,true (,no ,x)))
