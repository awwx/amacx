(use arcbase when findfile prn file-each eval)

(def runtest-if-exists (name target-module)
  (let src (findtest nil name)
    (when src
      (loadfile src target-module))))

(def loadfile (src target-module)
  (when (target-module '*inline-tests* nil)
    (prn "=> " src))

  (file-each (completepath rootdir src)
    (fn (x)
      (eval x target-module)))

  (when (target-module '*inline-tests* nil)
    (prn "<= " src)))
