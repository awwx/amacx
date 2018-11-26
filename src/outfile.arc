(use simple-def complex-fn open-output-file)

(def outfile (filename (o exists 'truncate))
  (open-output-file filename 'text exists))
