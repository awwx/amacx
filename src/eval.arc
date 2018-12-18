(use arcbase complex-fn eval-ail obj ac)

(def eval (x (o module this-container))
  (let compile (or (module 'compile-xVrP8JItk2Ot nil)
                   compile-xVrP8JItk2Ot)
    (let ailcode (compile module x)
      (if (a-namespace module)
          (eval-ail ailcode module)
          (eval-ail ailcode)))))
