; renamed from "loop" in Arc

(use mac w/uniq do rfn if)

(mac forloop (start test update . body)
  (w/uniq (gfn gparm)
    `(,do ,start
          ((,rfn ,gfn (,gparm)
             (,if ,gparm
                  (,do ,@body ,update (,gfn ,test))))
           ,test))))
