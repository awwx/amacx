(use arcbase complex-fn w/open)

(def readstr (s (o eof nil))
  (w/instring in s
    (readport in eof)))
