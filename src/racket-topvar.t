(use let racket-topvar)

(let container (racket-topvar-container)
  (aload "src/len.t" container))
