(use ns-var-rule copyinto prn ret obj implement-use load)

(def ns-var (target-module)
  (annotate 'mac
    (fn (varname)
      `(ns-var-xVrP8JItk2Ot ,varname))))

(def racket-topvar-container ()
  (ret m (ail-namespace)
    (copyinto m (ar-builtins))

    (sref m 'module-var (ns-var m))
    (sref m '*features* (cons 'module-var (m '*features*)))

    (sref m 'use      (implement-use m macro-expand))
    (sref m 'provides (implement-provides m))

    (load 'ns-var-rule m macro-expand)

    (load 'use-implementation m)))
