(use arcbase quasiquote for)

(mac repeat (n . body)
  `(,for ,(uniq) 1 ,n ,@body))
