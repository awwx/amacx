(use arcbase quasiquote foreach)

(mac each (var expr . body)
  `(,foreach ,expr (,fn (,var) ,@body)))
