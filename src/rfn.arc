(use mac let assign simple-fn)

(mac rfn (name parms . body)
  `(,let ,name nil
     (,assign ,name (,fn ,parms ,@body))))
