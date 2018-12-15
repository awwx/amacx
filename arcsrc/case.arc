(use arcbase quasiquote caselet)

(mac case (expr . args)
  `(,caselet ,(uniq) ,expr ,@args))
