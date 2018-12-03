(use mac simple-do map simple-fn setforms)

(mac set args
  `(,do ,@(map (fn (a) `(,= ,a ,t)) args)))
