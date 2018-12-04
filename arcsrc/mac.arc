(use arcboot annotate named-fn list)

; (assign mac
;   (annotate 'mac
;     (named-fn mac (name parms . body)
;       `(,assign ,name (,annotate 'mac (,named-fn ,name ,parms ,@body))))))

(assign mac
  (annotate 'mac
    (named-fn mac (name parms . body)
      (list assign name
            (list annotate ''mac
                  (cons named-fn (cons name (cons parms body))))))))
