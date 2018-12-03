(use simple-def when pr disp prn file-each eval)

(def loadfile (target-module src)
  (when (target-module '*inline-tests* nil)
    (pr "> ")
    (disp src)
    (prn))

  (file-each src
    (fn (x)
      (eval x target-module))))
