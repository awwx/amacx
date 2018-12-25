(use topvar)

; (mac set-module-var (var value)
;   `(,sref ,this-container ',var ,value))

(sref
  this-container
  ($quote set-module-var)
  (annotate ($quote mac)
    ($fn (var value)
      (cons sref
        (cons this-container
          (cons (cons ($quote $quote)
                      (cons var nil))
            (cons value nil)))))))
