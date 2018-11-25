(use simple-def do1 apply pr writec)

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))
