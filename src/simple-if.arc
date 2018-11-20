(use module-var mac quote)

(provisional if)

; (mac if args
;   `($if ,@args))

(mac if args
  (cons '$if args))
