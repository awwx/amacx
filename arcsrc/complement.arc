(use arcbase)

(mac complement (f)
  (let g (uniq)
    `(,fn ,g (,no (,apply ,f ,g)))))
