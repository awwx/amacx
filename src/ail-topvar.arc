(use copyinto prn ret obj implement-use load container setforms
     rule ac quasiquote)

(compiler-rule ail-topvar (caris e '$topvar--xVrP8JItk2Ot)
  e)

(assign ail-topvar-ac
  (gen-compiler `(ail-topvar ,@ac-rules)))

(= ail-topvar-macro
  (annotate 'mac
    (fn (varname)
      `($topvar--xVrP8JItk2Ot ,varname))))

(def ail-topvar-container ()
  (ret container (ail-namespace)
    (provision-container container (obj compiler ail-topvar-ac))
    (= container!topvar ail-topvar-macro)
    (provide-feature container 'topvar)))
