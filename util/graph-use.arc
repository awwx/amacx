(use findfile ssyntax setforms andf set unless +str when caris prn
     tostring file-each)

(= seen (table))

(def process (src)
  (unless (seen src)
    (when (a-sym src) (set seen.src))
    (let path (if (a-sym src)
                   (findsrc nil src)
                   (findfile rootdir *srcdirs* src))
      (when path
        (file-each (+ rootdir path)
          (fn (x)
            (when (caris x 'use)
              (each feature (cdr x)
                (prn (asfilename src) "(" (asfilename src) ") --> "
                     (asfilename feature))
                (process feature)))))))))

(def genhtml (chart)
  (w/outfile out "graph.html"
    (w/stdout out
      (prn #<<END
<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <script src="https://unpkg.com/mermaid@8.0.0-rc.8/dist/mermaid.min.js"></script>
    <script>mermaid.initialize({startOnLoad:true});</script></head>
    <style>
      body {
        margin: 3em;
      }
    </style>
  </head>
  <body>
    <div class="mermaid">
END
)

    (disp chart)

    (prn #<<END
    </div>
  </body>
</html>
END
))))

(unless argv
  (err "usage: graph-use <start>"))

(let chart
     (tostring
       (let in (car argv)
         (prn "graph TD")
         (process (if (some #\. in) in (strsym in)))))
  (genhtml chart))
