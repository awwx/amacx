(use def if no err
  ; todo
  fn)

(def len (xs (o l 0))
  (if (no xs)
       l
      (acons xs)
       (len (cdr xs) (+ l 1))
       (err "invalid list" xs)))
