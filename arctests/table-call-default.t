(use arcbase each obj equals)

(let g (obj a 1)
  (equals (g 'a)      1)
  (equals (g 'a 33)   1)
  (equals (g 'b)    nil)
  (equals (g 'b 33)  33))
