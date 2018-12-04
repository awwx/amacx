(use each prn unless is len ssyntax setforms px file-each +str)

(unless (is len.argv 1)
  (prn "Usage: uses.arc <feature>")
  (quit 1))

(= lookfor (strsym (car argv)))

(each srcdir *srcdirs*
  (each filename (dir srcdir)
    (file-each (+ rootdir "/" srcdir "/" filename)
      (fn (x)
        (when (and (caris x 'use)
                   (contains (cdr x) lookfor))
          (prn srcdir "/" filename))))))
