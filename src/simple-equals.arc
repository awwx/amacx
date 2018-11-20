(use mac list assert)

; (mac equals (a b)
;   `(,assert (,ar-iso ,a ,b)))

(mac equals (a b)
  (list assert (list ar-iso a b)))
