(use arcbase ret obj container)

(def ail-topvar-container ()
  (ret container (ail-namespace)
    (provision-container container
      (obj compiler (gen-compiler ac-rules)
           topvar   'ail))))
