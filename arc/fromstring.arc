(use arcbase w/uniq quasiquote w/stdout w/open)

(mac fromstring (str . body)
  (w/uniq gv
   `(,w/instring ,gv ,str
      (,w/stdin ,gv ,@body))))
