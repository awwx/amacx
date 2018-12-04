(use arcbase map)

(def mappend (f . args)
  (apply join (apply map f args)))
