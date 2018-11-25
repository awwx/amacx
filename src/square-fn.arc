(use mac quasiquote simple-fn prn)

(mac square-bracket args
  `(fn (_) (,@args)))
