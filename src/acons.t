(use module-var)

(ar-assert (is2 (acons (cons 1 2)) t))
(ar-assert (is2 (acons nil)        nil))
(ar-assert (is2 (acons 123)        nil))
