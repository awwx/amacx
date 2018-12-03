(use mac quasiquote simple-fn)

(mac square-bracket args
  `(,fn (_) (,@args)))
