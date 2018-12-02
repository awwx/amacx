(use simple-def w/open w/uniq whiler read)

(def file-each (filename f)
  (w/infile in filename
    (w/uniq eof
      (whiler x (read in eof) eof
        (f x)))))
