(use arcbase accum each complex-fn)

(def keys (h)
  (accum a (each (k v) h (a k))))
