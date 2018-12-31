(use arcbase curly-table curly-destruct obj equals square-fn)

(equals
  ((fn ({a})
     a)
   (obj a 10))
  10)

(equals
  ((fn ({a, b})
     (list a b))
   (obj a 11 b 12))
  '(11 12))

(equals
  ((fn (a {b})
     (list a b))
   13 (obj b 14))
  '(13 14))

(equals
  ((fn ({a} b)
     (list a b))
   (obj a 15) 16)
  '(15 16))

(equals
  ((fn ({a} (o b 17))
     (list a b))
   (obj a 18))
  '(18 17))

(equals
  ((fn ({a, b, c})
     (list a b c))
   (obj a 19 b 20 c 21))
  '(19 20 21))

(use string)

(equals
  ((fn ({a})
     a)
   string)
  "a")

(equals
  ((fn ({a 'k1})
     a)
   (obj k1 22))
  22)

(equals
  ((fn ({a 'k1, b 'k2})
     (list a b))
   (obj k1 23 k2 24))
  '(23 24))

(equals
  ((fn ({a 3})
     a)
   [+ _ 2])
  5)

(equals
  (let {a, b} (obj a 25 b 26)
    (list a b))
  '(25 26))

(equals
  (let {a 'x, b 'y} (obj x 27 y 28)
    (list a b))
  '(27 28))

(equals
  (with (a 29
         {b} (obj b 30)
         c 31)
    (list a b c))
  '(29 30 31))
