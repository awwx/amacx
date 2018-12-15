(use module-var named-fn)

(ar-assert (is2 (fnname (named-fn foo () 0))
                'foo))
