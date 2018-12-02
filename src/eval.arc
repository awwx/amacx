(use simple-def complex-fn or and has eval-ail obj)

(def eval (x
           (o module *module*)
           (o expander (or (and (has module 'macro-expand)
                                (has module 'eval)
                                (module 'macro-expand))
                           macro-expand)))
  (let ailcode (expander (obj module module
                              env '())
                         x)
    (if (a-namespace module)
         (eval-ail ailcode module)
         (eval-ail ailcode))))
