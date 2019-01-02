(use arcbase)

; Arc 3.2 ac.scm:954

(def as-sym (x)
  (if (a-sym x)
       x
      (a-str x)
       (strsym x)
      (a-char x)
       (strsym (charstr x))
       (err "Can't coerce to sym" x)))
