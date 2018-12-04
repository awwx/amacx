(use arcboot mac w/uniq let or map1 is)

(mac in (x . choices)
  (w/uniq g
    `(,let ,g ,x
       (,or ,@(map1 (fn (c) `(,is ,g ,c)) choices)))))
