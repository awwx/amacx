(use arcbase unless contains load each)

(def use-feature (container feature)
  (unless (has-feature container feature)
    (load feature container))
  nil)

; We want macros like this in the target container:
;
;     (mac use features
;       (each feature features
;         (use-feature this-container feature)))
;
;     (mac provides (feature)
;       (provide-feature this-container feature))
;
; however at the beginning of the load process with an empty
; container, `mac` and so on isn't defined yet, so we can't simply
; load these macro definitions inside the container.
;
; We could define these macros in our environment and copy them into
; the target container:
;
;     (= container!use use)
;     (= container!provides provides)
;
; however when loading the macro definitions into our environment,
; `this-container` would end up referring to *our* environment, not
; the target container... and features loaded in the target container
; would get loaded into *our* environment.
;
; Thus `implement-use` and `implement-provides`, when called with a
; target container, returns macros suitable for being copied into the
; target container:
;
;     (= container!use      (implement-use      container))
;     (= container!provides (implement-provides container))

(def implement-use (container)
  (annotate 'mac
    (fn features
      (each feature features
        (use-feature container feature))
      nil)))

(def implement-provides (container)
  (annotate 'mac
    (fn (feature)
      (provides-feature container feature)
      nil)))
