(use arcbase w/uniq do1 prn)

(mac time (expr)
  (w/uniq (t1 t2)
    `(,let ,t1 (,msec)
       (,do1 ,expr
             (,let ,t2 (,msec)
               (,prn "time: " (,- ,t2 ,t1) " msec."))))))
