(use arcboot mac quasiquote)

(mac square-bracket args
  `(,fn (_) (,@args)))
