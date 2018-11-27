(use rule alist apply join)

(rule + args (and (acons args) (alist (car args)))
  (apply join args))
