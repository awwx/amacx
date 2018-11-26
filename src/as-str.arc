(use simple-def complex-fn if or apply map1 inspect)

(def as-str (x (o radix))
  (if (a-str x)
       x
      (a-char x)
       (charstr x)
      (a-num x)
       (num-to-str x (or radix 10))
      (acons x)
       (apply str-append (map1 as-str x))
      (no x)
       ""
      (a-sym x)
       (sym-to-str x)
       (inspect x)))
