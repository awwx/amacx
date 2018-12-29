(use arcbase w/uniq quasiquote w/outstring w/stdout)

(mac tostring body
  (w/uniq gv
   `(,w/outstring ,gv
      (,w/stdout ,gv ,@body)
      (,inside ,gv))))

(mac errtostring body
  (w/uniq gv
   `(,w/outstring ,gv
      (,w/stderr ,gv ,@body)
      (,inside ,gv))))
