(use mac if simple-do)

(mac when (test . body)
  `(,if ,test (,do ,@body)))
