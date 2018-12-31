(use arcbase whilet disp write prn eval readstr complex-fn w/stdout)

(def repl ((o container this-container) (o prompt "> "))
  ; TODO some less obtrusive way of enabling block strings
  (w/splicing-port (stdin)
    (fn (in)
      (w/stdin in
        (whilet s (readline prompt)
          (readline-add-history s)
          (on-break
            (fn ()
              (prn))
            (fn ()
              (on-err
                (fn (e)
                  ; The stack trace has some useful information in the
                  ; srcloc runtime (though the output could be improved);
                  ; not much point in dumping the stack trace output
                  ; otherwise.
                  (if (is runtime 'srcloc)
                       ((error-display-handler) 'ignored e)
                       (do (disp "Error: ")
                           (write (details e)))))
                (fn ()
                  ; TODO we should have a way for a container to define
                  ; its own reader
                  (write (eval (readstr s) container))))
              (prn)))))))
  (prn))
