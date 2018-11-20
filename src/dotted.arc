(use simple-def atom and or)

(def dotted (x)
  (if (atom x)
       nil
       (and (cdr x) (or (atom (cdr x))
                        (dotted (cdr x))))))
