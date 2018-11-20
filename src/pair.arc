(use simple-def if no list cxr)

; (def pair (xs (o f list)) ...)

(def pair (xs . rs)
  ((fn (f)
     (if (no xs)
          nil
          (if (no (cdr xs))
               (list (list (car xs)))
               (cons (f (car xs) (cadr xs))
                     (pair (cddr xs) f)))))
   (if (is rs nil) list (car rs))))
