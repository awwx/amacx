(use and true)

(true (ar-iso (and)         t))
(true (ar-iso (and nil)     nil))
(true (ar-iso (and 1)       1))
(true (ar-iso (and nil 1)   nil))
(true (ar-iso (and 1 nil)   nil))
(true (ar-iso (and 1 2)     2))
(true (ar-iso (and nil 1 2) nil))
(true (ar-iso (and 1 2 nil) nil))
(true (ar-iso (and 1 2 3)   3))
