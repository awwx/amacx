(use simple-def when pr disp prn file-each eval)

(def runtest-if-exists (target-module name)
  (let src (findtest nil name)
    (when src
      (loadfile target-module src))))

(def loadfile (target-module src)
  (when (target-module '*inline-tests* nil)
    (pr "> ")
    (disp src)
    (prn))

  (file-each src
    (fn (x)
      (eval x target-module))))
