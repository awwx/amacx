(use module-var mac simple-if list quote)

; (mac do body
;   `(($fn () ,@body))

(mac do body
  (if (is2 body nil)
        nil
        (list (cons '$fn (cons '() body)))))
