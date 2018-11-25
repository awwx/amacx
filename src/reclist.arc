(use simple-def and or)

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))
