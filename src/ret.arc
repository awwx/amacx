(use mac quasiquote let)

(mac ret (var val . body)
  `(,let ,var ,val ,@body ,var))
