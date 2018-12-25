(use topvar set-module-var)

; (mac quote (x)
;   `($quote ,x))

($assign quote
  (annotate ($quote mac)
    ($fn (x)
      (cons ($quote $quote) (cons x nil)))))
