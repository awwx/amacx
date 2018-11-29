(use let table true sets-iso setforms ssyntax keys some find
     square-fn iso let cxr equals find)

(let g (table)
  (true (sets-iso (keys g) nil))

  (= g!a 1)
  (true (sets-iso (keys g) '(a)))

  (= g!b 2)
  (true (sets-iso (keys g) '(a b)))

  (= g!c 3)
  (true (sets-iso (keys g) '(a b c)))

  (= (g '(x y)) 4)
  (true (sets-iso (keys g) '(a b c (x y)))))
