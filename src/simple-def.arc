(use arcboot mac named-fn)

; (mac def (name parms . body)
;   `(,assign ,name (,named-fn ,name ,parms ,@body)))

(mac def (name parms . body)
  (cons assign
    (cons name
      (cons (cons named-fn (cons name (cons parms body)))
            nil))))
