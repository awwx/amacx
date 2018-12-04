(use arcbase)

(mac withs (parms . body)
  (if (no parms)
      `(,do ,@body)
      `(,let ,(car parms) ,(cadr parms)
         (,withs ,(cddr parms) ,@body))))
