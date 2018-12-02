(use mac w/uniq rfn when)

(mac while (test . body)
  (w/uniq (gf gp)
    `((,rfn ,gf (,gp)
        (,when ,gp ,@body (,gf ,test)))
      ,test)))
