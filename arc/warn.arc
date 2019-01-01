(use arcbase disp each write)

; Arc 3.2 arc.arc:308

; TODO stderr?

(def warn (msg . args)
  (disp (+ "Warning: " msg ". ") (stderr))
  (each x args
    (write x (stderr))
    (disp " " (stderr)))
  (disp #\newline (stderr)))
