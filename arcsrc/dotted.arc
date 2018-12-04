(use arcbase)

(def dotted (x)
  (if (atom x)
       nil
       (and (cdr x) (or (atom (cdr x))
                        (dotted (cdr x))))))
