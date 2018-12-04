(use arcboot mac quasiquote call-w/stdout)

(mac w/stdout (str . body)
  `(call-w/stdout ,str (fn () ,@body)))
