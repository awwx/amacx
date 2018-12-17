(use simple-equals caris)

(equals (caris 15    15) nil)
(equals (caris '(15) 16) nil)
(equals (caris '(15) 15) t)
