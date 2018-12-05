(use arcbase w/uniq quasiquote w/outstring w/stdout)

(mac tostring body
  (w/uniq gv
   `(,w/outstring ,gv
      (,w/stdout ,gv ,@body)
      (,inside ,gv))))
