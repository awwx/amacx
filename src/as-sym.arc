(def as-sym (x)
  (if (a-sym x)
       x
      (a-str x)
       (strsym x)
       (err "Can't coerce to sym" x)))
