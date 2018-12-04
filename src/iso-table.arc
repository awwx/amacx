(use arcbase sets-iso keys all square-fn iso ssyntax rule)

(def table-equal (t1 t2)
  (and (sets-iso (keys t1) (keys t2))
       (all [iso t1._ t2._] (keys t1))))

(rule iso (x y) (and (isa x 'table) (isa y 'table))
  (table-equal x y))
