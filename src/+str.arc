(use rule + and apply string)

(rule + args (and (acons args) (or (a-str (car args)) (a-char (car args))))
  (apply string args))
