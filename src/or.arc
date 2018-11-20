(use mac and let uniq list)

; (mac or args
;   (and args
;        (w/uniq g
;          `(let ,g ,(car args)
;             (if ,g ,g (or ,@(cdr args)))))))

(mac or args
  (and args
       (let g (uniq)
         (list let g (car args)
            (list if g g (cons or (cdr args)))))))
