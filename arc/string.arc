(use arcbase as-str)

(def string args
  (apply str-append (map1 as-str args)))
