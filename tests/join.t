(use assert join)

(assert (ar-iso (join '(a b) '(c d))
                '(a b c d)))

(assert (ar-iso (join '(a b) 'c)
                '(a b . c)))
