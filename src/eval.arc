(use simple-def complex-fn or and has eval-ail obj)

(def eval (x
           (o module *module*)
           (o expander (or (and (has module 'macro-expand)
                                (has module 'eval)
                                (module 'macro-expand))
                           macro-expand)))
  (eval-ail (expander (obj module module
                           env '())
                      x)))
