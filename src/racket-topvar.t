(use let racket-topvar)

(let container (racket-topvar-container)
  (load "src/len.t" container))
