(use mac if no simple-do)

(mac unless (test . body)
  `(,if (,no ,test) (,do ,@body)))
