(use arcbase obj equals even square-fn andf square-fn
     true false compose complement andf)

(with (x (obj a 1 b 2 c 3)
       i 'b)
  (equals x!a 1)
  (equals x.i 2))

(let plus1 [+ _ 1]
  (equals (even&plus1 1) nil)
  (equals (even&plus1 2) 3)
  (equals (even&plus1 3) nil))

(false (an-int&even nil))
(false (an-int&even 'foo))
(false (an-int&even 3))
(true  (an-int&even 4))

(equals (~ nil) t)
(equals (~ t)   nil)

(equals (~acons '(a)) nil)
(equals (~acons 123)  t)

(with (5+ [+ _ 5]
       7+ [+ _ 7])
  (equals (5+:7+    6)     18)
  (equals (5+:7+:5+ 6)     23)
  (equals (5+:7+:*  4 6 7) 180))

(equals (~acons:car '(123))   t)
(equals (~acons:car '((123))) nil)
