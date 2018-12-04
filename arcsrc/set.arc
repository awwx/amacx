(use arcboot mac simple-do map setforms)

(mac set args
  `(,do ,@(map (fn (a) `(,= ,a ,t)) args)))
