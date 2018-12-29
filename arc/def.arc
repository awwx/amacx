(use arcbase quasiquote safeset named-fn)

(provides simple-def)

(assign sig (table))

(mac def (name parms . body)
  `(do (sref sig ',name ',parms)
       (safeset ,name (,named-fn ,name ,parms ,@body))))
