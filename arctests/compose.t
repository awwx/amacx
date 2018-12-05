(use arcbase equals compose even square-fn)

(equals ((compose)             '(a b c)) '(a b c))
(equals ((compose car)         '(a b c)) 'a)
(equals ((compose car cdr)     '(a b c)) 'b)
(equals ((compose car cdr cdr) '(a b c)) 'c)

(equals ((compose +)         1 2 3 4)   10)
(equals ((compose even +)    1 2 3 4)   t)
(equals ((compose even +)    1 2 3 4 1) nil)
(equals ((compose no even +) 1 2 3 4)   nil)
(equals ((compose no even +) 1 2 3 4 1) t)

(with (5+ [+ _ 5]
       7+ [+ _ 7])
  (equals ((compose 5+ 7+) 6)       18)
  (equals ((compose 5+ 7+ 5+) 6)    23)
  (equals ((compose 5+ 7+ *) 4 6 7) 180))

; todo ssyntax
; (let seq "abcd"
;   (equals (idfn:seq 2) #\c))
