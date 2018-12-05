(use arcbase w/uniq withs rev)

(mac accum (accfn . body)
  (w/uniq gacc
    `(,withs (,gacc nil ,accfn (,fn (x) (,assign ,gacc (,cons x ,gacc))))
       ,@body
       (,rev ,gacc))))
