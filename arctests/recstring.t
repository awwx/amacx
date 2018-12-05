(use arcbase equals recstring square-fn)

(equals (recstring [if (is ("abc" _) #\x) 'found] "abc")   nil)
(equals (recstring [if (is ("axc" _) #\x) 'found] "axc")   'found)
(equals (recstring [if (is ("axc" _) #\x) 'found] "axc" 1) 'found)
(equals (recstring [if (is ("axc" _) #\x) 'found] "axc" 2) nil)
