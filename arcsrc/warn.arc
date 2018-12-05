(use arcbase disp each write)

(def warn (msg . args)
  (disp (+ "Warning: " msg ". ") (stderr))
  (each x args
    (write x (stderr))
    (disp " " (stderr)))
  (disp #\newline (stderr)))
