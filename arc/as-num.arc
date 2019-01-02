(use arcbase complex-fn aif)

(def as-num (x (o radix))
  (if (or (a-num x) (an-int x))
       x
      (a-str x)
       (or (strnum x (or radix 10))
           (err "Can't coerce to num" x))
      (err "Can't coerce num" x)))
