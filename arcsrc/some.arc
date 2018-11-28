(use simple-def let testify if reclist recstring compose)

(def some (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist (compose f car) seq)
        (recstring (compose f seq) seq))))
