; For when we don't have mem yet

(use arcboot)

(def contains (lst x)
  (if (no lst)
       nil
      (is (car lst) x)
       t
       (contains (cdr lst) x)))
