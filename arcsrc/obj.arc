(use arcbase listtab complex-fn)

(mac obj args
  `(,listtab (,list ,@(map1 (fn ((k v))
                              `(,list ',k ,v))
                           (pair args)))))
