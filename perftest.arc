(use simple-def if is - equals let prn time reduce +)

; We don't have setforms yet, so a version of n-of which
; simply repeats the same value.

(def nvs (n v)
  (if (is n 0)
       '()
       (cons v (nvs (- n 1) v))))

(equals (nvs 3 1) '(1 1 1))

(let xs (nvs 1000000 1)
  (prn (time (reduce + xs))))
