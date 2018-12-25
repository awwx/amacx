(use topvar set-topvar quote)

; (mac assign args
;   `($assign ,@args))

($assign assign
  (annotate 'mac
    ($fn args
      (cons '$assign args))))
