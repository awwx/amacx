(use arc equals iso-table)

(let x (obj a 1 b 2 c 3 d 4 e 5 f 6)
  (rotate x!a x!b x!c)
  (equals x (obj a 2 b 3 c 1 d 4 e 5 f 6)))
