(use arc mlist equals)

; TODO here I'm imagining we might want to have a runtime where lists
; are immutable by default (and thus we'd need to make a mutable copy
; of the list for the test).  However in Arc lists should be mutable
; by default.  Perhaps tests could have a way of checking which
; features are loaded, and `arc` being present in the feature list
; would tell us that we're in Arc's universe.

(let x (mcopy '(a b c d e f g))
  (swap (x 2) (x 4))
  (equals x '(a b e d c f g)))
