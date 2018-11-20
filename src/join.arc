(use simple-def if no let)

(def join args
  (if (no args)
       nil
      (let a (car args)
        (if (no (cdr args))
             a
            (no a)
             (ar-apply join (cdr args))
             (cons (car a) (ar-apply join (cons (cdr a) (cdr args))))))))
