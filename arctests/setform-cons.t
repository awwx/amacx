(use arcbase setform-cons equals listset mlist)

(mac test-setform (orig mod val expected)
  `(let x (,mcopy ',orig)
     (= ,mod ,val)
     (equals x ',expected)))

(test-setform (a b c)       (x 0)                     9  (9 b c))
(test-setform (a b c)       (x 1)                     9  (a 9 c))
(test-setform (a b c)       (x 2)                     9  (a b 9))
(test-setform (a b)         (car x)                   3  (3 b))
(test-setform ((a b) (c d)) (car (car x))             4  ((4 b) (c d)))
(test-setform ((a b) (c d)) (caar x)                  5  ((5 b) (c d)))
(test-setform ((a b) (c d)) (car:car x)               6  ((6 b) (c d)))
(test-setform ((a b) (c d)) (car (cdr (car (cdr x)))) 7  ((a b) (c 7)))
(test-setform (a b c d e)   (x 2)                     8  (a b 8 d e))
