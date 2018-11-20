(use mac foreach fn)

(mac each (var expr . body)
  `(,foreach ,expr (,fn (,var) ,@body)))
