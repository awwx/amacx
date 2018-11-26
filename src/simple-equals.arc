(use mac list true)

; (mac equals (a b)
;   `(,true (,ar-iso ,a ,b)))

(mac equals (a b)
  (list true (list ar-iso a b)))
