(use arcbase copyinto implement-use load)

(def provision-container (container config)
  (with (builtins       (or (config 'builtins)       (ar-builtins))
         macro-expander (or (config 'macro-expander) macro-expand)
         inline-tests   (config 'inline-tests)
         start          (config 'start))
    (copyinto container builtins)
    (when inline-tests
      (sref container '*inline-tests* t))
    (sref container 'use      (implement-use      container))
    (sref container 'provides (implement-provides container))
    (when start
      (load start container macro-expander))
    container))
