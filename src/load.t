(use equals accum load)

(equals
  (accum a
    (readfile-each (str-append rootdir "samples/sample1") a))
  '((a (b) c) nil 1 2 3))
