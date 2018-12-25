(use topvar set-topvar)

; (mac quote (x)
;   `($quote ,x))

($assign quote
  (annotate ($quote mac)
    ($fn (x)
      (cons ($quote $quote) (cons x nil)))))
