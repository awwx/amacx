(use topvar set-topvar assign quote)

; (mac fn (parms . body)
;   `($fn ,parms ,@body))

(assign fn
  (annotate 'mac
    ($fn (parms . body)
      (cons '$fn (cons parms body)))))
