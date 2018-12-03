(use mac quasiquote call-w/stdout simple-fn)

(mac w/stdout (str . body)
  `(call-w/stdout ,str (fn () ,@body)))
