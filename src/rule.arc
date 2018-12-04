(use arcbase w/uniq quasiquote aif)

(mac rule (name arglist test . body)
  (w/uniq (orig args)
    `(,let ,orig ,name
       (,assign ,name
         (,fn ,args
           (,aif (,apply (,fn ,arglist ,test) ,args)
                  (,apply (,fn ,arglist ,@body) ,args)
                  (,apply ,orig ,args)))))))
