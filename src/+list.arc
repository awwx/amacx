(use arcbase rule)

(rule + args (and (acons args) (alist (car args)))
  (apply join args))
