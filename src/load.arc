(use arcbase contains unless complex-fn when findfile loadfile)

(def has-feature (container feature)
  (contains (container '*features* nil) feature))

(def provide-feature (container feature)
  (unless (has-feature container feature)
    (sref container '*features*
      (cons feature (container '*features* nil))))
  nil)

(def load (name (o target-container this-container))
  (when (a-sym name)
    (provide-feature target-container name))

  (let src (if (a-sym name)
                (findsrc target-container name)
                name)
    (unless src
      (err "load: unable to find in srcdirs:" name))
    (loadfile src target-container))

  (when (and (a-sym name)
             (target-container '*inline-tests* nil))
    (runtest-if-exists name target-container)))
