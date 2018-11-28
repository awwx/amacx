(use simple-def aif and isa ret pair mac simple-do case)

(def $ail-rename (x)
  (case x
    $quote   'quote-xVrP8JItk2Ot
    $fn      'fn-xVrP8JItk2Ot
    $assign  'assign-xVrP8JItk2Ot
    $if      'if-xVrP8JItk2Ot
    $call    'call-xVrP8JItk2Ot))

(def replace-tree (x mapping)
  (aif (and (isa x 'sym) (mapping x))
        it
       (acons x)
        (cons (replace-tree (car x) mapping)
              (replace-tree (cdr x) mapping))
        x))

(mac $ail body
  `(do ,@(replace-tree body $ail-rename)))
