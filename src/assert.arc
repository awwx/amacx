(use mac list if simple-do quote)

(mac assert (x)
  (list if x
         (list do (list ar-disp "OK " '(stdout))
                  (list ar-write (list quote x) '(stdout))
                  (list ar-disp #\newline '(stdout)))
         (list err "FAIL" (list quote x))))
