(use mac let assign fn)

(mac rfn (name parms . body)
  `(,let ,name nil
     (,assign ,name (,fn ,parms ,@body))))
