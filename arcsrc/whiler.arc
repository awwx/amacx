(use arcboot mac w/uniq withs testify while no)

(mac whiler (var expr endval . body)
  (w/uniq gf
    `(,withs (,var nil ,gf (,testify ,endval))
       (,while (,no (,gf (,assign ,var ,expr)))
         ,@body))))
