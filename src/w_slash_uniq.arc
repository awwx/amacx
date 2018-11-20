(use mac if with apply join map1 let uniq)

(mac w/uniq (names . body)
  (if (acons names)
      `(,with ,(apply join nil (map1 (fn (n) `(,n (,uniq ',n)))
                                 names))
         ,@body)
      `(,let ,names (,uniq ',names) ,@body)))
