(use simple-def if no)

(def map1 (f xs)
  (if (no xs)
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))
