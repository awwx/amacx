(use arcbase +str square-fn reclist compose asfilename)

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

(assign *srcdirs*
  '("arc" "arctests" "extras" "qq" "qqtests" "src" "xboot"))

(def findsrc (container name)
  (findfile rootdir
            *srcdirs*
            (str-append (asfilename name) ".arc")))

(def findtest (container name)
  (findfile rootdir
            *srcdirs*
            (str-append (asfilename name) ".t")))
