(use simple-def if no)

(def combine-apply (args)
  (if (no args)
       nil
      (no (cdr args))
       (car args)
       (cons (car args) (combine-apply (cdr args)))))
