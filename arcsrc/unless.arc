(use mac quasiquote if no simple-do)

(mac unless (test . body)
  `(,if (,no ,test) (,do ,@body)))
