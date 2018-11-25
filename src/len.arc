(use simple-def complex-fn if no err)

(def len (xs (o l 0))
  (if (no xs)
       l
      (acons xs)
       (len (cdr xs) (+ l 1))
      (a-str xs)
       (ar-strlen xs)
       (err "invalid list" xs)))
