(use arcbase contains unless complex-fn when findfile loadfile)

(def has-feature (module feature)
  (contains (module '*features* nil) feature))

(def provides-feature (module feature)
  (unless (has-feature module feature)
    (sref module '*features*
      (cons feature (module '*features* nil))))
  nil)

(def load (name (o target-module this-container))
  (when (a-sym name)
    (provides-feature target-module name))

  (let src (if (a-sym name)
                (findsrc nil name)
                name)
    (unless src
      (err "load: unable to find in srcdirs:" name))
    (loadfile src target-module))

  (when (and (a-sym name)
             (target-module '*inline-tests* nil))
    (runtest-if-exists name target-module)))
