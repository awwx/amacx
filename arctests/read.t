(use equals read)

(equals (read "foo")
        'foo)

(equals (w/infile in "sample"
          (read in))
        '(a (b) c))
