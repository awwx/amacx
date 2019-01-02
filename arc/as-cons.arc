(use arcbase)

(def as-cons (x)
  (if (a-str x)
       (strchars x)
       (err "Can't coerce" x)))
