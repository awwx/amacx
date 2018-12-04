(use arcboot mac let)

(mac afn (parms . body)
  `(,let self nil
     (,assign self (,fn ,parms ,@body))))
