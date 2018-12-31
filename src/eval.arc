(use arcbase complex-fn obj ac)

(def eval (x (o container this-container))
  (let compile (or (container 'compile--xVrP8JItk2Ot nil)
                   compile--xVrP8JItk2Ot)
    (let ailcode (compile container x)
      (if (a-namespace container)
          (eval-ail ailcode container)
          (eval-ail ailcode)))))
