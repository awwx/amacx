(use simple-def if no)

(def uniq args
  (if (no args)
       (ar-uniq nil nil)
       (ar-uniq nil (car args))))
