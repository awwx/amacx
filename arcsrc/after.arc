(use arcbase)

(mac after (x . ys)
  `(,protect (,fn () ,x) (,fn () ,@ys)))
