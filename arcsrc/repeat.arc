(use mac for uniq)

(mac repeat (n . body)
  `(,for ,(uniq) 1 ,n ,@body))