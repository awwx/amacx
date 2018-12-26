(use arcbase complex-fn)

(def write (x (o port (stdout)))
  (ar-write x port)
  nil)
