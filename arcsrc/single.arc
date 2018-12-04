(use arcbase)

(def single (x)
  (and (acons x)
       (no (cdr x))))
