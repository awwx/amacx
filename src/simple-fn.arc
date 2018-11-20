(use module-var set-module-var assign)

(provisional fn)

; (mac fn (parms . body)
;   `($fn ,parms ,@body))

(assign fn
  (annotate 'mac
    ($fn (parms . body)
      (cons '$fn (cons parms body)))))
