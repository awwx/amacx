(use arcbase complex-fn eval-ail obj macro)

(def eval (x (o module *module*))
  (let expander (or (module 'macro-expand nil) macro-expand)
    (let ailcode (expander (obj module module
                                env '())
                           x)
      (if (a-namespace module)
          (eval-ail ailcode module)
          (eval-ail ailcode)))))
