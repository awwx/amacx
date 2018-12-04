(use arcboot mac list)

; (mac do body
;   `(($fn () ,@body))

(mac do body
  ($if (is2 body nil)
        nil
        (list (cons '$fn (cons '() body)))))
