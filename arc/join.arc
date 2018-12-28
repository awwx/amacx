(use simple-def if no let)

(def join args
  (if (no args)
       nil
      (let a (car args)
        (if (no (cdr args))
             a
            (no a)
             (apply1 join (cdr args))
             (cons (car a) (apply1 join (cons (cdr a) (cdr args))))))))
