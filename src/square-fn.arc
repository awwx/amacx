(use arcbase quasiquote)

(mac square-bracket args
  `(,fn (_) (,@args)))
