(use simple-def mac map1 unless has table contains load each)

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
; (mac use features
;   (each feature features
;     (use-feature feature *module* macro-expand)))
;
; except that if we imported such a `use` into a target container,
; using `use` in the target container would load things into *our*
; source container, not the target container.
;
; Thus `implement-use` here, when called with a target container,
; returns a macro suitable for being injected into the target
; container.  For example,
;
; (= target!use (implement-use target macro-expand))

(def implement-use (container expander)
  (annotate 'mac
    (fn features
      (each feature features
        (use-feature feature container expander))
      nil)))

(def implement-provides (container)
  (annotate 'mac
    (fn (feature)
      (provides-feature container feature))))
