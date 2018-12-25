(use topvar)

(ar-assert (is2 ($if nil 172 203) 203))
(ar-assert (is2 ($if t   101  55) 101))
