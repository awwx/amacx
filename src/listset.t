(use arcbase listset mlist equals)

(let x (mcopy '(a b c d))
  (listset x 0 'w)
  (equals x '(w b c d))
  (listset x 1 'x)
  (equals x '(w x c d)))
