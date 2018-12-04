(use simple-def mac map1 unless has table contains load)

(def use-feature (feature container expander)
  (unless (contains (container '*features* nil) feature)
    (load feature container expander))
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
; Thus `implement-use`, when called with a target container, returns
; a macro suitable for being injected into the target container.
; For example,
;
; (= target!use (implement-use target))

(def implement-use (container expander)
  (annotate 'mac
    (fn args
      ; we don't have `each` yet in the boot process...
      (map1 (fn (feature)
              (use-feature feature container expander))
            args)
      nil)))

(def implement-provides (container)
  (annotate 'mac
    (fn (feature)
      (provides-feature container feature))))
