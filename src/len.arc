(use arcbase complex-fn)

(def len (xs (o l 0))
  (if (no xs)
       l
      (acons xs)
       (len (cdr xs) (+ l 1))
      (a-str xs)
       (ar-strlen xs)
      (a-table xs)
       (ar-table-len xs)
       (err "invalid list" xs)))
