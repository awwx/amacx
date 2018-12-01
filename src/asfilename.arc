(use assign obj simple-def string map1 aif strchars as-str)

(assign convert-filename-chars
  (obj #\/ "slash"
       #\\ "backslash"
       #\_ "underline"
       #\< "lt"
       #\> "gt"))

(def asfilename (s)
  (string
    (map1 (fn (c)
            (aif (convert-filename-chars c)
                  (string "_" it "_")
                  c))
         (strchars (as-str s)))))
