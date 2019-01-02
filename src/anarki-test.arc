(use arc equals)

(use prn pr) ; TODO remove when in arc

(= assert-same equals)

(mac anarki-suite (name . body)
  `(,do (prn "==== " ',name " ====")
        ,@body))

(mac anarki-test (name . body)
  `(,do (,pr ',name ": ")
        ,@body))
