(use arcbase when findfile prn file-each eval)

(def runtest-if-exists (name target-module expander)
  (let src (findtest nil name)
    (when src
      (loadfile src target-module expander))))

(def loadfile (src target-module expander)
  (when (target-module '*inline-tests* nil)
    (prn "> " src))

  (file-each src
    (fn (x)
      (eval x target-module expander))))
