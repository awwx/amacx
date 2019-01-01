(use arcbase equals assoc)

(let check (fn (a b)
             (equals (alref '((a 1) (b 2) (c 3)) a)
                     b))
  (check 'a 1)
  (check 'b 2)
  (check 'c 3)
  (check 'x nil))
