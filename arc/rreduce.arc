(use arcbase)

(def rreduce (f xs)
  (if (cddr xs)
       (f (car xs) (rreduce f (cdr xs)))
       (apply f xs)))
