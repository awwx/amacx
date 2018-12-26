(use arcbase do1 pr writec)

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))
