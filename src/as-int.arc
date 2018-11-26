(use simple-def complex-fn aif or)

(def as-int (x (o radix))
  (if (an-int x)
       x
      (a-num x)
       (iround x)
      (a-char x)
       (charcode x)
      (a-str x)
       (aif (str-to-num x (or radix 10))
             (iround it)
             (err "Can't coerce to int" x))
       (err "Can't coerce to int" x)))
