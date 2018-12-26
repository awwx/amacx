(use arcbase quasiquote)

(mac afn (parms . body)
  `(,let self nil
     (,assign self (,fn ,parms ,@body))))
