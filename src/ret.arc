(use arcbase quasiquote)

(mac ret (var val . body)
  `(,let ,var ,val ,@body ,var))
