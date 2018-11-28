(use mac let uniq simple-fn no apply)

(mac complement (f)
  (let g (uniq)
    `(,fn ,g (,no (,apply ,f ,g)))))
