(use arcbase rule ac quasiquote)

(compiler-rule namespace-topvar (caris e '$ns-var--xVrP8JItk2Ot)
  e)

(assign ns-topvar-ac
  (gen-compiler `(namespace-topvar ,@ac-rules)))
