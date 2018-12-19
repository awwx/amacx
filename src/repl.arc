(use arcbase whilet write prn eval readstr complex-fn)

(def repl ((o prompt "> "))
  (whilet s (readline prompt)
    (readline-add-history s)
    (on-break
      (fn ()
        (prn))
      (fn ()
        (on-err
          (fn (e)
            ((error-display-handler) (details e) e))
          (fn ()
            (write (eval (readstr s)))))
        (prn))))
  (prn))
