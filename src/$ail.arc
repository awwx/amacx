(use arcboot simple-def replace-tree case mac quasiquote simple-do)

(def rename-$ail (x)
  (replace-tree x
    (fn (m)
      (case m
        $quote   'quote-xVrP8JItk2Ot
        $fn      'fn-xVrP8JItk2Ot
        $assign  'assign-xVrP8JItk2Ot
        $if      'if-xVrP8JItk2Ot
        $call    'call-xVrP8JItk2Ot))))

(mac $ail body
  `(do ,@(rename-$ail body)))
