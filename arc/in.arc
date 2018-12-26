(use arcbase w/uniq)

(mac in (x . choices)
  (w/uniq g
    `(,let ,g ,x
       (,or ,@(map1 (fn (c) `(,is ,g ,c)) choices)))))
