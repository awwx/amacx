(use arcbase quasiquote)

(mac w/uniq (names . body)
  (if (acons names)
      `(,with ,(apply join nil (map1 (fn (n) `(,n (,uniq ',n)))
                                 names))
         ,@body)
      `(,let ,names (,uniq ',names) ,@body)))
