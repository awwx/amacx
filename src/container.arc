(use arcbase copyinto implement-use load)

(def provision-container (container config)
  (with (builtins       (or (config 'builtins) (ar-builtins))
         compiler       (or (config 'compiler) compile--xVrP8JItk2Ot)
         inline-tests   (config 'inline-tests)
         start          (config 'start))

    (copyinto container builtins)

    (when inline-tests
      (sref container '*inline-tests* t))

    (sref container 'use      (implement-use      container))
    (sref container 'provides (implement-provides container))
    (sref container 'compile--xVrP8JItk2Ot compiler)

    (sref container 'options--xVrP8JItk2Ot
      (obj validate-ail (config 'validate-ail)))

    (case (or (config 'topvar nil)
              (err "topvar not specified in options"))
      ail (load 'ail-topvar container)
      ref (load 'ref-topvar container)
          (err "unknown topvar" (config 'topvar)))

    (when start
      (load start container))

    container))

(def new-container ()
  (provision-container (symtab) (obj)))
