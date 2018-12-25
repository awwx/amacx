(use topvar set-module-var assign quote)

; (mac fn (parms . body)
;   `($fn ,parms ,@body))

(assign fn
  (annotate 'mac
    ($fn (parms . body)
      (cons '$fn (cons parms body)))))
