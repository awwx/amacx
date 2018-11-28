(use mac protect simple-fn)

(mac after (x . ys)
  `(,protect (,fn () ,x) (,fn () ,@ys)))
