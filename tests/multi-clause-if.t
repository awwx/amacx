(ar-assert (is (if) nil))
(ar-assert (is (if nil 1) nil))
(ar-assert (is (if t 1) 1))
(ar-assert (is (if nil 1 2) 2))
(ar-assert (is (if t 1 2) 1))
(ar-assert (is (if nil 1 t 2) 2))
(ar-assert (is (if nil 1 nil 2) nil))
(ar-assert (is (if nil 1 nil 2 3) 3))
(ar-assert (is (if nil 1 nil 2 nil 3) nil))
(ar-assert (is (if nil 1 nil 2 t 3) 3))
(ar-assert (is (if nil 1 nil 2 nil 3 4) 4))
