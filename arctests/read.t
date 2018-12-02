(use equals read)

(equals (read "foo")
        'foo)

(equals (w/infile in (str-append rootdir "sample")
          (read in))
        '(a (b) c))
