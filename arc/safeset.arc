(use arcbase when bound disp)

(def warn-on-redef (var)
  (when (bound var)
    (disp "*** redefining " (stderr))
    (disp var (stderr))
    (disp #\newline (stderr))))

(mac safeset (var val)
  `(,do (,warn-on-redef ',var)
        (,assign ,var ,val)))
