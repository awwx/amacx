(use arcbase some complement testify)

; we don't have ssyntax yet

(def all (test seq)
  (no (some (complement (testify test)) seq)))
