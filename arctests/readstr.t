(use equals readstr)

(equals (readstr "")      nil)
(equals (readstr "" 'eof) 'eof)
(equals (readstr "123")   123)
(equals (readstr "foo")   'foo)

(equals (cadr (readstr "(a b c)"))
        'b)
