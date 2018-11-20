(use mac if list)

; (mac and args
;   (if args
;       (if (cdr args)
;           `(,if ,(car args) (,and ,@(cdr args)))
;           (car args))
;       t))

(mac and args
  (if args
      (if (cdr args)
           (list if (car args) (cons and (cdr args)))
           (car args))
      t))
