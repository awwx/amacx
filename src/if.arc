(use mac no list cxr)

; (mac if args
;   ($if (no args)
;         nil
;         (no cdr args)
;          (car args)
;          `($if ,(car args)
;                 ,(cadr args)
;                 (,if ,@(cddr args)))))

(mac if args
  ($if (no args)
    nil
    ($if (no (cdr args))
      (car args)
      (list '$if (car args)
                  (cadr args)
                  (cons if (cddr args))))))
