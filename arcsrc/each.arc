(use mac quasiquote foreach simple-fn)

(mac each (var expr . body)
  `(,foreach ,expr (,fn (,var) ,@body)))
