(use simple-def combine-apply)

(def apply (f . args)
  (apply1 f (combine-apply args)))
