(provides topvar)

; (mac topvar (var)
;   `($topvar ,var))
;
; -->
;
; (sref this-container 'topvar
;   (annotate 'mac
;     (fn (var)
;       (cons (quote $topvar)
;         (cons var nil))))
;
; -->

(($topvar sref) this-container ($quote topvar)
  (($topvar annotate) ($quote mac)
    ($fn (var)
      (($topvar cons) ($quote $topvar)
        (($topvar cons) var ($quote nil))))))
