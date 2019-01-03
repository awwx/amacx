(use equals accum load)

(equals
  (accum a
    (readfile-each (str-append rootdir "sample") a))
  '((a (b) c) nil 1 2 3))
