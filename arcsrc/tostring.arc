(use mac w/uniq quasiquote w/outstring w/stdout inside)

(mac tostring body
  (w/uniq gv
   `(,w/outstring ,gv
      (,w/stdout ,gv ,@body)
      (,inside ,gv))))
