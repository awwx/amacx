(use simple-def apply str-append map as-str)

(def string args
  (apply str-append (map as-str args)))
