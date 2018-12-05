(use arcbase for)

(mac repeat (n . body)
  `(,for ,(uniq) 1 ,n ,@body))
