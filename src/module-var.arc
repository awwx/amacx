; Bootstrap!
;
; (mac module-var (var)
;   `(,*module* ($quote ,var)))
;
; -->
;
; (sref *module* 'module-var
;   (annotate 'mac
;     (fn (var)
;       (cons *module*
;         (cons (cons '$quote (cons var nil))
;           nil)))))
;
; -->

((*module* ($quote sref))
  *module*
  ($quote module-var)
  ((*module* ($quote annotate))
    ($quote mac)
    ($fn (var)
      ((*module* ($quote cons))
       *module*
       ((*module* ($quote cons))
        ((*module* ($quote cons))
         ($quote $quote)
         ((*module* ($quote cons))
          var
          ($quote nil)))
        ($quote nil))))))
