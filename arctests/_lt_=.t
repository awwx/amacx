(use <= true false)

(true  (<=))
(true  (<= 3))

(true  (<= 3 3))
(true  (<= 3 4))
(false (<= 4 3))

(true  (<= 1 3 3))
(true  (<= 3 3 3))
(false (<= 3 4 3))
(true  (<= 3 3 4))
(false (<= 4 3 3))
(false (<= 4 4 3))
