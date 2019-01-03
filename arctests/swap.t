(use arc mlist equals)

; TODO mutable-list-assumption

(let x (copylist '(a b c d e f g))
  (swap (x 2) (x 4))
  (equals x '(a b e d c f g)))
