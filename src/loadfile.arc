(use arcbase w/open w/uniq whiler read findfile prn eval)

(def readfile-each (filename f)
  (w/infile in filename
    (w/splicing-port in
      (fn (splicing-port)
        (w/uniq eof
          (whiler x (read splicing-port eof) eof
            (f x)))))))

(def runtest-if-exists (name target-container)
  (let src (findtest target-container name)
    (when src
      (loadfile src target-container))))

(def loadfile (src target-container)
  (when (target-container '*inline-tests* nil)
    (prn "=> " src))

  (readfile-each (completepath rootdir src)
    (fn (x)
      (eval x target-container)))

  (when (target-container '*inline-tests* nil)
    (prn "<= " src)))
