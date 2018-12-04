(use arcbase equals testify)

(equals ((testify 3)     3)   t)
(equals ((testify 3)     4)   nil)
(equals ((testify alist) nil) t)
(equals ((testify alist) 3)   nil)
