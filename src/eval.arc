(use simple-def complex-fn eval-ail obj)

(def eval (x
           (o module *module*)
           (o expander (or (and (has module 'macro-expand)
                                (module 'macro-expand))
                           macro-expand)))
  (eval-ail (expander (obj module module
                           env '())
                      x)))
