(use arcbase <)

(def >= args
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))
