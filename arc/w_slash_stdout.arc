(use arcbase quasiquote)

(mac w/stdin (str . body)
  `(,call-w/stdin ,str (,fn () ,@body)))

(mac w/stdout (str . body)
  `(,call-w/stdout ,str (,fn () ,@body)))

(mac w/stderr (str . body)
  `(,call-w/stderr ,str (,fn () ,@body)))
