(use arcbase prn pr time container)

(let container (table)
  (provision-container container (obj))
  (use-features container '(arcbase))
  (pr "use arc: ")
  (time (use-features container '(arc))))
