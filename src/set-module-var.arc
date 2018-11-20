(use module-var)

; (mac set-module-var (var value)
;   `(,sref ,*module* ',var ,value))

((*module* ($quote sref))
  *module*
  ($quote set-module-var)
  (annotate ($quote mac)
    ($fn (var value)
      (cons sref
        (cons *module*
          (cons (cons ($quote $quote)
                      (cons var nil))
            (cons value nil)))))))
