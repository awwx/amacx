(use mac caselet uniq)

(mac case (expr . args)
  `(,caselet ,(uniq) ,expr ,@args))
