(use arcbase quasiquote)

(mac rfn (name parms . body)
  `(,let ,name nil
     (,assign ,name (,fn ,parms ,@body))))
