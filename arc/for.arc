(use arcbase w/uniq forloop <)

; Arc 3.2 arc.arc:452

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(,with (,v nil ,gi ,init ,gm (,+ ,max 1))
       (,forloop (,assign ,v ,gi) (,< ,v ,gm) (,assign ,v (,+ ,v 1))
         ,@body))))
