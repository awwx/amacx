(use mac if do)

(mac when (test . body)
  `(,if ,test (,do ,@body)))
