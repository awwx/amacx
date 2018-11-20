(use def combine-apply)

(def apply (f . args)
  (ar-apply f (combine-apply args)))
