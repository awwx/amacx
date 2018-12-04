(use arcboot mappend quasiquote pair obj)

(mac curly-bracket args
  `(obj ,@(mappend (fn (kv)
                     (if (cdr kv)
                          kv
                          (list (car kv) (car kv))))
                   args)))
