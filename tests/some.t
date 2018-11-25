(use equals some simple-fn or is no)

(equals (some #\c "abcdef") t)
(equals (some #\x "abcdef") nil)

(equals (some (fn (x) (or (is x #\a) (is x #\b)))
              "def")
        nil)

(equals (some (fn (x) (or (is x #\a) (is x #\b)))
              "dbf")
        t)

(equals (some 2 '())        nil)
(equals (some 3 '(1 2 3 4)) t)
(equals (some 5 '(1 2 3 4)) nil)

(equals (some no '(t t t t)) nil)
(equals (some no '(nil))     t)
