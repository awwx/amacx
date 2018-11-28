(use > true false)

(true (>))
(true  (> 1))

(false  (> 1 2))
(false  (> 1 1))
(true   (> 2 1))

(false (> 1 2 3))
(false (> 1 1 1))
(false (> 1 2 1))
(false (> 2 1 3))
(true  (> 3 2 1))
