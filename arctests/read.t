(use equals read)

(equals (read "foo")
        'foo)

(equals (w/infile in (str-append rootdir "samples/sample1")
          (read in))
        '(a (b) c))
