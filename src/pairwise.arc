(use simple-def if no cxr)

(def pairwise (pred lst)
  (if (no lst)
       t
      (no (cdr lst))
       t
      (pred (car lst) (cadr lst))
       (pairwise pred (cdr lst))
       nil))
