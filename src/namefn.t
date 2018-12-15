(use module-var simple-fn)

(ar-assert (is2 (fnname (namefn 'foo (fn () 0)))
                'foo))
