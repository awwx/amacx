(use arcbase)

(assign mlist list)

(def mcopy (x)
  (if (acons x)
       (cons (mcopy (car x))
             (mcopy (cdr x)))
       x))
