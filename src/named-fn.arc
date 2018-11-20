(use module-var set-module-var assign simple-fn)

; (mac named-fn (name parms . body)
;   `(,namefn ',name (,fn ,parms ,@body)))

(assign named-fn
  (annotate 'mac
    (fn (name parms . body)
      (cons namefn
        (cons (cons quote (cons name nil))
          (cons (cons fn (cons parms body))
                nil))))))
