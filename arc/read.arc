(use arcbase complex-fn readstr)

(def read ((o x (stdin)) (o eof nil))
  (if (a-str x) (readstr x eof) (readport x eof)))
