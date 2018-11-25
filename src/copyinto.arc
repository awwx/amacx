; For when we don't have copy yet.

(use simple-def each complex-fn)

(def copyinto (g . srcs)
  (each src srcs
    (each (k v) src
      (sref g k v)))
  g)
