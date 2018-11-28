(use mac simple-fn map1 pair cxr)

; (mac with (parms . body)
;   `((,fn ,(map1 car (pair parms))
;      ,@body)
;     ,@(map1 cadr (pair parms))))

(mac with (parms . body)
  (cons
   (cons fn (cons (map1 car (pair parms)) body))
   (map1 cadr (pair parms))))
