(use equals let +)

(equals
  (let xyz 0
    (assign xyz (+ xyz 1)))
  1)
