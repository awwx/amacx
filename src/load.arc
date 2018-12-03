(use simple-def contains unless complex-fn when findfile
     if loadfile and)

(def has-feature (module feature)
  (contains (module '*features* nil) feature))

(def add-feature (module feature)
  (unless (has-feature module feature)
    (sref module '*features*
      (cons feature (module '*features* nil)))))

(def load (name (o target-module *module*))
  (when (a-sym name)
    (add-feature target-module name))

  (let src (if (a-sym name)
                (findsrc nil name)
                name)
    (loadfile target-module src))

  (when (and (a-sym name)
             (target-module '*inline-tests* nil))
    (runtest-if-exists target-module name)))
