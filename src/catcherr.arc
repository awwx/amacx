(use arcbase quasiquote)

(mac catcherr (expr)
  `(on-err (fn (c) (list 'err (details c)))
           (fn () ,expr)))
