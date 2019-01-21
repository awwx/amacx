(use arcbase complex-fn obj ac)

(def eval (x
           (o container this-container)
           (o compiler (or (container 'compile--xVrP8JItk2Ot nil)
                           compile--xVrP8JItk2Ot))
           (o options (container 'options--xVrP8JItk2Ot nil)))
  (let ailcode (compiler container x options)
    (if (a-namespace container)
        (eval-ail ailcode container)
        (eval-ail ailcode))))
