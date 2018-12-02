(use equals accum file-each)

(equals
  (accum a
    (file-each (str-append rootdir "sample") a))
  '((a (b) c) nil 1 2 3))
