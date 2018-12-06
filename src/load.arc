(use arcbase contains unless complex-fn when findfile loadfile)

(def has-feature (module feature)
  (contains (module '*features* nil) feature))

(def add-feature (module feature)
  (unless (has-feature module feature)
    (sref module '*features*
      (cons feature (module '*features* nil)))))

(def load (name
           (o target-module *module*)
           (o expander (or (target-module 'macro-expand nil)
                           (*module* 'macro-expand))))
  (when (a-sym name)
    (add-feature target-module name))

  (let src (if (a-sym name)
                (findsrc nil name)
                name)
    (unless src
      (err "load: unable to find in srcdirs:" name))
    (loadfile src target-module expander))

  (when (and (a-sym name)
             (target-module '*inline-tests* nil))
    (runtest-if-exists name target-module expander)))
