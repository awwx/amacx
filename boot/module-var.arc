; Bootstrap!
;
; (mac module-var (var)
;   `(,this-container ($quote ,var)))
;
; -->
;
; (sref this-container 'module-var
;   (annotate 'mac
;     (fn (var)
;       (cons this-container
;         (cons (cons '$quote (cons var nil))
;           nil)))))
;
; -->

((this-container ($quote sref))
  this-container
  ($quote module-var)
  ((this-container ($quote annotate))
    ($quote mac)
    ($fn (var)
      ((this-container ($quote cons))
       this-container
       ((this-container ($quote cons))
        ((this-container ($quote cons))
         ($quote $quote)
         ((this-container ($quote cons))
          var
          ($quote nil)))
        ($quote nil))))))
