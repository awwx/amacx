(use each list obj equals)

(let g (symtab (obj a 1))
  (equals (g 'a)      1)
  (equals (g 'a 33)   1)
  (equals (g 'b 33)  33))
