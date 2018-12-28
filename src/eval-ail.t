(use equals $ail)

($ail
  (equals (eval-ail '($quote 42))
          42)

  (equals (eval-ail `($call ($quote ,+) ($quote 1) ($quote 2)))
          3))
