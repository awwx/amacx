(use simple-def apply-args)

(def apply (f . args)
  (apply1 f (apply-args args)))
