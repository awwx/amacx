(use simple-def if no)

; Arc 3.2 ac.scm:689

(def apply-args (args)
  (if (no args)
       nil
      (no (cdr args))
       (car args)
       (cons (car args) (apply-args (cdr args)))))
