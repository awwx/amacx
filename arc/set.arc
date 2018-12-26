(use arcbase map setforms)

(mac set args
  `(,do ,@(map (fn (a) `(,= ,a ,t)) args)))
