(use true iso join no)

(true     (iso (join '(a b) '(c d)) '(a b c d)))
(true (no (iso (join '(a b) '(c d)) '(a b c x))))
