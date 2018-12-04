(use arcboot mac let uniq no apply)

(mac complement (f)
  (let g (uniq)
    `(,fn ,g (,no (,apply ,f ,g)))))
