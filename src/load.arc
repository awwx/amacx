(use arcbase w/open w/uniq whiler read prn eval contains unless
     complex-fn when findfile)

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

  (readfile-each src
    (fn (x)
      (eval x target-container)))

  (when (target-container '*inline-tests* nil)
    (prn "<= " src)))

(def has-feature (container feature)
  (contains (container '*features* nil) feature))

(def provide-feature (container feature)
  (unless (has-feature container feature)
    (sref container '*features*
      (cons feature (container '*features* nil))))
  nil)

(def load (name (o target-container this-container))
  (when (a-sym name)
    (provide-feature target-container name))

  (let src (if (a-sym name)
                (findsrc target-container name)
                name)
    (unless src
      (err "load: unable to find in srcdirs:" name))
    (loadfile src target-container))

  (when (and (a-sym name)
             (target-container '*inline-tests* nil))
    (runtest-if-exists name target-container)))
