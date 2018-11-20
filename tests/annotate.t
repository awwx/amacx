(use let annotate equals)

(let x (annotate 'foo 123)
  (equals (ar-tag-type x) 'foo)
  (equals (rep x)  123))
