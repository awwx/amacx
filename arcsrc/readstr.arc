(use arcbase complex-fn w/open readport)

(def readstr (s (o eof nil))
  (w/instring in s
    (readport in eof)))
