(use mac w/uniq withs simple-fn assign rev)

(mac accum (accfn . body)
  (w/uniq gacc
    `(,withs (,gacc nil ,accfn (,fn (x) (,assign ,gacc (,cons x ,gacc))))
       ,@body
       (,rev ,gacc))))
