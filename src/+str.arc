(use rule + and or apply string)

(rule + args (and (acons args) (or (a-str (car args)) (a-char (car args))))
  (apply string args))
