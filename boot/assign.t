(use assign)

(ar-assert (is2 (($fn (a)
                   ($assign a 42)
                   a)
                 99)
                42))

(ar-assert (is2 (($fn (a)
                   (assign a 42))
                 0)
                42))
