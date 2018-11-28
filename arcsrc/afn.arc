(use mac let assign simple-fn)

(mac afn (parms . body)
  `(,let self nil
     (,assign self (,fn ,parms ,@body))))
