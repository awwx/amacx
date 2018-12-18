(use arcbase when findfile prn file-each eval)

(def runtest-if-exists (name target-container)
  (let src (findtest nil name)
    (when src
      (loadfile src target-container))))

(def loadfile (src target-container)
  (when (target-container '*inline-tests* nil)
    (prn "=> " src))

  (file-each (completepath rootdir src)
    (fn (x)
      (eval x target-container)))

  (when (target-container '*inline-tests* nil)
    (prn "<= " src)))
