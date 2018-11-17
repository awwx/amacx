(assert     (iso (join '(a b) '(c d)) '(a b c d)))
(assert (no (iso (join '(a b) '(c d)) '(a b c x))))
