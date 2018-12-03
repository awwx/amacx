(use mac quasiquote let outstring)

(mac w/outstring (var . body)
  `(,let ,var (outstring) ,@body))
