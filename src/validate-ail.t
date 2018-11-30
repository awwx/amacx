(use $ail true validate-ail)

($ail
  (true (ail-expr '($fn (x)
                     ($call ($quote +) x ($quote 10))))))
