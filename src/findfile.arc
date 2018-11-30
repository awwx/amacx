(use simple-def if is +str and file-exists square-fn reclist compose)

(def completepath (basedir path)
  (if (is (path 0) #\/)
       path
       (+ basedir "/" path)))

(def listsome (f seq)
  (reclist (compose f car) seq))

(def findfile (basedir dirs filename)
  (listsome [and (file-exists (+ (completepath basedir _) "/" filename))
                 (+ _ "/" filename)]
        dirs))
