(use arcboot equals type annotate table stdout stdin)

(equals (type (annotate 'foo 123)) 'foo)
(equals (type '(1 . 2))            'cons)
(equals (type 'foo)                'sym)
(equals (type (fn () 0))           'fn)
(equals (type #\A)                 'char)
(equals (type "foo")               'string)
(equals (type 123)                 'int)
(equals (type 1.23)                'num)
(equals (type (table))             'table)
(equals (type (stdout))            'output)
(equals (type (stdin))             'input)

; todo socket, exception, thread
