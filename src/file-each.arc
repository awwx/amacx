(use arcbase w/open w/uniq whiler read)

(def file-each (filename f)
  (w/infile in filename
    (w/splicing-port in
      (fn (splicing-port)
        (w/uniq eof
          (whiler x (read splicing-port eof) eof
            (f x)))))))
