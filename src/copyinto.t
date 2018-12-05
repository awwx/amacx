(use arcbase copyinto obj equals)

(let g (copyinto (obj a 1 b 2 c 3 d 4)
                 (obj b 88)
                 (obj a 77 d 99))
  (equals (g 'a) 77)
  (equals (g 'b) 88)
  (equals (g 'c) 3)
  (equals (g 'd) 99))
