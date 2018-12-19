(use arcbase equals recstring)

; don't have square brackets yet

(equals (recstring (fn (_) (if (is ("abc" _) #\x) 'found)) "abc")   nil)
(equals (recstring (fn (_) (if (is ("axc" _) #\x) 'found)) "axc")   'found)
(equals (recstring (fn (_) (if (is ("axc" _) #\x) 'found)) "axc" 1) 'found)
(equals (recstring (fn (_) (if (is ("axc" _) #\x) 'found)) "axc" 2) nil)
