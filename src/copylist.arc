(use arcbase)

(def copylist (xs)
  (if (no xs)
      nil
      (cons (car xs) (copylist (cdr xs)))))
