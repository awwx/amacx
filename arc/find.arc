(use arcbase testify reclist recstring ssyntax square-fn)

(def find (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))
