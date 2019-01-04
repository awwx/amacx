(use arc equals)

(let a '(1 3 6 9 15)
  (insort < 7 a)
  (equals a '(1 3 6 7 9 15)))
