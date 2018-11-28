(use simple-def apply join map)

(def mappend (f . args)
  (apply join (apply map f args)))
