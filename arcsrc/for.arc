(use arcboot mac w/uniq with + forloop <)

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(,with (,v nil ,gi ,init ,gm (,+ ,max 1))
       (,forloop (,assign ,v ,gi) (,< ,v ,gm) (,assign ,v (,+ ,v 1))
         ,@body))))
