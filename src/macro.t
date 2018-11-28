(use equals macro obj unless)

(equals (macro (obj) 'foo) nil)
(equals (macro (obj unless unless) 'unless) unless)
