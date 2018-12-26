(use arcbase w/uniq quasiquote ret)

(mac do1 args
  (w/uniq g
    `(,ret ,g ,(car args)
       ,@(cdr args))))
