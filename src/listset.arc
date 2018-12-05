(use arcbase scxr)

(def listset (xs i v)
  (if (is i 0)
       (scar xs v)
       (listset (cdr xs) (- i 1) v)))
