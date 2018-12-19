(use arcbase w/uniq rfn when)

(mac whilet (var test . body)
  (w/uniq (gf gp)
    `((,rfn ,gf (,gp)
        (,let ,var ,gp
          (,when ,var ,@body (,gf ,test))))
      ,test)))
