(use arcbase complex-fn)

(def as-str (x (o radix))
  (if (a-str x)
       x
      (a-char x)
       (charstr x)
      (a-num x)
       (numstr x (or radix 10))
      (acons x)
       (apply str-append (map1 as-str x))
      (no x)
       ""
      (a-sym x)
       (symstr x)
       (inspect x)))
