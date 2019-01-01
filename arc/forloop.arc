; renamed from "loop" in Arc

(use arcbase w/uniq rfn)

; Arc 3.2 arc.arc:444

(mac forloop (start test update . body)
  (w/uniq (gfn gparm)
    `(,do ,start
          ((,rfn ,gfn (,gparm)
             (,if ,gparm
                  (,do ,@body ,update (,gfn ,test))))
           ,test))))
