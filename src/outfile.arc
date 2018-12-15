(use arcbase complex-fn)

(def outfile (filename (o exists 'truncate))
  (open-outfile filename 'text exists))
