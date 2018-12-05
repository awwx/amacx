(use arcbase complex-fn)

(def outfile (filename (o exists 'truncate))
  (open-output-file filename 'text exists))
