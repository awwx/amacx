(use arcbase quasiquote)

(mac w/stdout (str . body)
  `(call-w/stdout ,str (fn () ,@body)))
