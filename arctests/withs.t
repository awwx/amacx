(use arcbase simple-equals withs +)

(equals (withs (a 1)) nil)

(equals (withs (a 1 b (+ a 1))
          (list a b))
  '(1 2))

(equals (withs (a 1 b (+ a 1) c (+ a b))
          (list a b c))
  '(1 2 3))
