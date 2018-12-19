(use arcbase equals ++ obj ssyntax setforms setform-cons mlist)

(let a 3
  (equals (++ a) 4)
  (equals a 4))

(let x (mlist 1 2 3)
  (++ (car x))
  (equals x '(2 2 3)))

(let a (obj a 1 b 2 c 3)
  (equals (++ a!c) 4)
  (equals a!c 4))

(let a 3
  (equals (-- a) 2)
  (equals a 2))

(let a (obj a 1 b 2 c 3)
  (equals (-- a!c) 2)
  (equals a!c 2))
