(use arcbase quasiquote)

(mac aif (expr . body)
  `(,let it ,expr
     (,if it
         ,@(if (cddr body)
               `(,(car body) (,aif ,@(cdr body)))
               body))))
