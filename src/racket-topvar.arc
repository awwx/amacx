(use ns-var-rule copyinto prn ret obj implement-use load container
     setforms ssyntax)

(= ns-topvar
  (annotate 'mac
    (fn (varname)
      `(ns-var-xVrP8JItk2Ot ,varname))))

(def racket-topvar-container ()
  (ret container (ail-namespace)
    (provision-container container (obj))
    (= container!module-var ns-topvar)
    (provides-feature container 'module-var)))
