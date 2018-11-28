(use mac with list)

; (mac let (var val . body)
;   `(,with (,var ,val) ,@body))

(mac let (var val . body)
  (cons with (cons (list var val) body)))
