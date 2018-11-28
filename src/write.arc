(use simple-def complex-fn ar-write)

(def write (x (o port (stdout)))
  (ar-write x port))
