(use arcbase quasiquote)

(mac w/outstring (var . body)
  `(,let ,var (outstring) ,@body))
