(use mac listtab list map1 complex-fn pair)

(mac obj args
  `(,listtab (,list ,@(map1 (fn ((k v))
                              `(,list ',k ,v))
                           (pair args)))))
