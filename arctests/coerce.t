(use arcbase equals coerce)

(equals (coerce 5        'int)     5)
(equals (coerce "123"    'int)     123)
(equals (coerce "123.8"  'int)     124)
(equals (coerce "10"     'int 16)  16)
(equals (coerce #\A      'int)     65)

(equals (coerce 5        'num)     5)
(equals (coerce "123"    'num)     123)
(equals (coerce "123.8"  'num)     123.8)
(equals (coerce "10"     'num 16)  16)
(equals (coerce "10.8"   'num 16)  16.5)

(equals (coerce 5        'string)  "5")
(equals (coerce 5.2      'string)  "5.2")
(equals (coerce nil      'string)  "")
(equals (coerce 'foo     'string)  "foo")
(equals (coerce '(a b c) 'string)  "abc")

(equals (coerce "foo"    'sym)     'foo)
(equals (coerce #\A      'sym)     'A)

(equals (coerce "abc"    'cons)    '(#\a #\b #\c))
