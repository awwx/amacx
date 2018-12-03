(use ns-var-rule copyinto prn ret obj implement-use)

(def ns-var (target-module)
  (annotate 'mac
    (fn (varname)
      `(ns-var-xVrP8JItk2Ot ,varname))))

(def racket-topvar-container ()
  (ret m (ail-namespace)
    (copyinto m (ar-builtins))

    (sref m 'module-var (ns-var m))
    (sref m '*features* (cons 'module-var (m '*features*)))

    (sref m 'use      (implement-use m))
    (sref m 'provides (implement-provides m))

    (aload 'file-each m *module*)
    (aload 'macro m *module*)
    (aload 'ns-var-rule m *module*)
    (aload 'asfilename m *module*)
    (aload 'findfile m *module*)
    (aload 'implement-use m *module*)
    (aload 'eval m *module*)))
