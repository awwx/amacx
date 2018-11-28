(use simple-def and no)

(def single (x)
  (and (acons x)
       (no (cdr x))))
