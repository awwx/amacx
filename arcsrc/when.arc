(use arcbase)

(mac when (test . body)
  `(,if ,test (,do ,@body)))
