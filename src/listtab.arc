(use def let table map1 fn sref)

(def listtab (al)
  (let h (table)
    (map1 (fn ((k v)) (sref h k v))
          al)
    h))
