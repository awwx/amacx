(use topvar)

; (mac set-topvar (var value)
;   `(,sref ,this-container ',var ,value))

(sref
  this-container
  ($quote set-topvar)
  (annotate ($quote mac)
    ($fn (var value)
      (cons sref
        (cons this-container
          (cons (cons ($quote $quote)
                      (cons var nil))
            (cons value nil)))))))
