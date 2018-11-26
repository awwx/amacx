(use equals as-str)

(equals (as-str "foo")  "foo")
(equals (as-str #\X)    "X")
(equals (as-str 123)    "123")
(equals (as-str 123 16) "7b")
(equals (as-str 3.141)  "3.141")
(equals (as-str nil)    "")
(equals (as-str 'foo)   "foo")
(equals (as-str +)      "#<procedure:+>")
