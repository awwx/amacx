(use arcbase whilet write prn eval readstr complex-fn)

(def repl ((o container this-container) (o prompt "> "))
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
            ; TODO we should have a way for a container to define
            ; its own reader
            (write (eval (readstr s) container))))
        (prn))))
  (prn))
