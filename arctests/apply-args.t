(use simple-equals apply-args)

(equals (apply-args '((1 2 3)))  '(1 2 3))
(equals (apply-args '(1 (2 3)))  '(1 2 3))
(equals (apply-args '(1 2 (3)))  '(1 2 3))
(equals (apply-args '(1 2 3 ())) '(1 2 3))
