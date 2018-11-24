(use equals combine-apply)

(equals (combine-apply '((1 2 3)))  '(1 2 3))
(equals (combine-apply '(1 (2 3)))  '(1 2 3))
(equals (combine-apply '(1 2 (3)))  '(1 2 3))
(equals (combine-apply '(1 2 3 ())) '(1 2 3))
