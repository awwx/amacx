(use arcboot mac protect)

(mac after (x . ys)
  `(,protect (,fn () ,x) (,fn () ,@ys)))
