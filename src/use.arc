(use simple-def mac map1 unless has table contains)

(def use-feature (container feature)
  (unless (has container '*features*)
    (sref container '*features* (table)))
  (unless (contains (container '*features*) feature)
    (aload feature
           container
           (if (and (has container 'macro-expand)
                    (has container 'findfile))
               container
               *module*)))
  nil)

(def provides-feature (container feature)
  (unless (has container '*features*)
    (sref container '*features* nil))
  (unless (contains (container '*features*) feature)
    (sref container '*features* (cons feature (container '*features*))))
  nil)

; This is like
;
; (mac use args
;   (each arg args (use-feature *module* arg)))
;
; except that if we imported this version of `use` into a target
; container, the `*module*` would refer to the *source* container,
; not the *target* container.
;
; Thus `use-implementation`, when called with a target container,
; returns a macro function suitable for being injected into the
; target container, annotated as a macro.  For example,
;
; (= target!use (annotate 'mac (use-implementation target)))

(def use-implementation (container)
  (fn args
    ; we don't have `each` yet in the boot process...
    (map1 (fn (arg)
            (use-feature container arg))
          args)
    nil))

(def provides-implementation (container)
  (fn (feature)
    (provides-feature container feature)))
