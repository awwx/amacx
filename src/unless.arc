(use mac if no do)

(mac unless (test . body)
  `(,if (,no ,test) (,do ,@body)))
