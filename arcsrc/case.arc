(use arcbase caselet)

(mac case (expr . args)
  `(,caselet ,(uniq) ,expr ,@args))
