(use mac atomic-invoke simple-fn)

(mac atomic body
  `(,atomic-invoke (,fn () ,@body)))

(mac atlet args
  `(,atomic (,let ,@args)))

(mac atwith args
  `(,atomic (,with ,@args)))

(mac atwiths args
  `(,atomic (,withs ,@args)))
