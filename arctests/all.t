(use arcbase equals all in)

(equals (all #\a "")     t)
(equals (all #\a "aaaa") t)
(equals (all #\a "aaba") nil)

(equals (all (fn (c) (in c #\a #\b)) "")      t)
(equals (all (fn (c) (in c #\a #\b)) "aabba") t)
(equals (all (fn (c) (in c #\a #\b)) "aabca") nil)

(equals (all 'a '(a a a))       t)
(equals (all 'a '(a b a))       nil)

(equals (all no '())            t)
(equals (all no '(a))           nil)
(equals (all no '(nil))         t)
(equals (all no '(nil nil nil)) t)
