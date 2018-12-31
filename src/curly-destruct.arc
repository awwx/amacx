(use arcbase complex-fn rule map)

(def fn-curly-arg (args)
  (and (acons args)
       (caris (car args) 'curly-bracket)))

(rule fn-complex-args? (args) (fn-curly-arg args)
  t)

(rule fn-complex-args (args ra) (fn-curly-arg args)
  (join
    (let kvs (cdar args)
      (map (fn (kv)
             (if (cdr kv)
                  `(,(car kv) ((car ,ra) ,(cadr kv)))
                  `(,(car kv) ((car ,ra) ',(car kv)))))
           kvs))
    (fn-complex-args (cdr args) `(,cdr ,ra))))
