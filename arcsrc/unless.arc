(use arcbase quasiquote)

(mac unless (test . body)
  `(,if (,no ,test) (,do ,@body)))
