(use true join)

(true (ar-iso (join '(a b) '(c d))
              '(a b c d)))

(true (ar-iso (join '(a b) 'c)
              '(a b . c)))
