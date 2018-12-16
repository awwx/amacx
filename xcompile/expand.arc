(mac test body
  `(do ,@body))

(= caddr car:cddr)

(def xhash (x)
  (if (isa x 'table)
       '<table>
      (acons x)
       (cons (xhash (car x))
             (xhash (cdr x)))
       x))

(mac px (x)
  (w/uniq (gx)
    `(let ,gx ,x
       (write ',x)
       (disp ": ")
       (write (xhash ,gx))
       (disp #\newline)
       ,gx)))

(mac catcherr (expr)
  `(on-err (fn (c) (list 'err (details c)))
           (fn () ,expr)))

(mac equals (x expected)
  (w/uniq (actual expect)
    `(with (,actual (catcherr ,x) ,expect ,expected)
       (if (iso ,actual ,expect)

            (do (disp "OK ")
                (write ',x)
              (disp " => ")
              (write (xhash ,actual))
              (disp "\n"))

            (do (disp "FAIL ")
                (write ',x)
              (disp " => ")
              (disp "\n")
              (write (xhash ,actual))
              (disp " NOT")
              (disp "\n")
              (write (xhash ,expect))
              (disp "\n")
              (quit 1))))))

(test
  (equals (+ 1 2) 3))

(mac true (x)
  `(if ,x
        (do (disp "OK ")
            (write ',x)
            (disp "\n"))

        (do (disp "FAIL ")
            (write ',x)
            (disp "\n")
            (quit 1))))

(mac ret (var val . body)
  `(let ,var ,val ,@body ,var))

(test
  (equals (ret a nil
            (= a 3)
            nil)
          3))

(= as-str   [coerce _ 'string])
(= strchars [coerce _ 'cons])

(def str-append args
  (apply + "" args))

(= ar-print prn)

(def hasloc (x) nil)
(def srcloc (loc x) x)

(mac use args)

(load "../src/asfilename.arc")
(load "../src/asfilename.t")

(load "../src/findfile.arc")
(load "../src/findfile.t")

(load "../src/replace-tree.arc")

(load "../src/$ail.arc")
(load "../src/$ail.t")

(load "../src/validate-ail.arc")
(load "../src/validate-ail.t")

(load "../src/contains.arc")

(load "../src/macro.arc")
(load "../src/macro.t")

(each bootfile (dir "../boot")
  (ensure-dir "../xboot")
  (let in (readfile (+ "../boot/" bootfile))
    (let out (rename-$ail in)
      (w/outfile o (+ "../xboot/" bootfile)
        (each x out
          (write x o)
          (disp "\n" o))))))

($ail
  (def ailarc (x)
    (if (isa x 'sym)
         x

        (acons x)
         (case (car x)
           $quote  `(quote ,(x 1))
           $assign `(assign ,(x 1) ,(ailarc (x 2)))
           $fn     `(fn ,(x 1) ,@(map ailarc (cddr x)))
           $if     `(if ,@(map ailarc (cdr x)))
           $call   (map ailarc (cdr x))
                   (err "don't recognize ail expr" x))

         (err "don't recognize ail expr" x)))

  (test
    (equals (ailarc '($quote foo))
            ''foo)

    (equals (ailarc 'foo) 'foo)
    (equals (ailarc '($assign foo bar)) '(assign foo bar))

    (equals (ailarc '($fn (x) y ($quote 3)))
            '(fn (x) y '3))

    (equals (ailarc '($if a b c))
            '(if a b c))

    (equals (ailarc '($call a b c))
            '(a b c))))

(def expand-eval-arc (context x)
  (eval:ailarc:macro-expand context x))

($ail
  (test
    (= zilch (obj module (obj)))

    (equals (expand-eval-arc zilch '($quote 88)) 88)

    (equals (expand-eval-arc zilch `(($quote ,+)
                                     ($quote 3)
                                     ($quote 4)))
            7)

    (equals ((fn (call) (call))
             (fn () 42))
            42)))

(def ar-assert (v)
  (if v
       (prn "OK")
       (do (prn "FAIL")
           (quit 1))))

(def print args
  (each arg args
    (write arg)
    (disp " "))
  (prn)
  (car args))

(def builtins ()
  (obj acons         [isa _ 'cons]
       annotate      annotate
       apply         apply
       ar-apply      apply
       ar-assert     ar-assert
       ar-disp       disp
       ar-iso        iso
       is2           is
       ar-print      print
       ar-strlen     len
       ar-symstr     [coerce _ 'string]
       ar-tag-type   type
       ar-uniq       ar-uniq
       ar-write      write
       ar-writec     writec
       ar-<2         <
       a-char        [isa _ 'char]
       a-fn          [isa _ 'fn]
       a-namespace   (fn (x) nil)
       a-num         [or (isa _ 'num) (isa _ 'int)]
       a-str         [isa _ 'string]
       a-sym         [isa _ 'sym]
       a-table       [isa _ 'table]
       a-tagged      a-tagged
       an-input      [isa _ 'input]
       an-int        [isa _ 'int]
       an-output     [isa _ 'output]
       call-w/stdout call-w/stdout
       car           car
       cdr           cdr
       charstr       [coerce _ 'string]
       close         close
       cons          cons
       details       details
       eval-ail      eval:ailarc
       err           err
       file-exists   file-exists
       fnname        fnname
       has           has
       hasloc        hasloc
       infile        infile
       inside        inside
       inspect       (fn (x) (tostring (write x)))
       instring      instring
       mod           mod
       namefn        namefn
       numstr        (fn (x n) (coerce x 'string n))
       open-outfile  (fn (filename . args) (outfile filename))
       open-socket   open-socket
       outstring     outstring
       protect       protect
       readport      sread
       rep           rep
       rootdir       rootdir
       srcloc        srcloc
       sref          (fn (g k v) (sref g v k))
       stderr        stderr
       stdin         stdin
       stdout        stdout
       str-append    (fn args (apply + "" args))
       strchars      [coerce _ 'cons]
       symstr        [coerce _ 'string]
       t             t
       table         table
       table-each    (fn (g f) (maptable f g))
       +             +
       -             -
       *             *
       /             /))

(def create-boot-module ()
  (ret boot-module (builtins)
    (= boot-module!*features* (keys boot-module))))

(def context (module)
  (obj module module validate validate-ail))

(def findval (g val)
  (catch
    (each (k v) g
      (when (eq? v val)
        (throw k)))
    (err "fn not found in module" val)))

(test
  (equals (findval (obj a 1 b 2 c 3) 2) 'b))

(def munch (module x)
  (if (eq? x module)
       (box '*module*)

      (isa x 'fn)
       (box (findval module x))

      (acons x)
       (cons (munch module (car x))
             (munch module (cdr x)))

       x))

(let module (create-boot-module)
  (test
    (equals (tostring (write (munch module `(a ,module b))))
            "(a #&*module* b)")

    (equals (tostring (write (munch module `(a ,module!cons b))))
            "(a #&cons b)")))

(def execf (module out src x)
  (let m (macro-expand (context module) x)
    (when out
      (disp "\n; " out)
      (write src out)
      (disp "\n\n" out)
      (write (munch module m) out)
      (disp "\n\n" out))
    (eval (ailarc m))))

(def use-feature (inline-tests module out feature)
  (unless (mem feature module!*features*)
    (xload inline-tests module out feature)))

(def process-use (inline-tests module out x)
  (each feature (cdr x)
    (use-feature inline-tests module out feature)))

(def add-feature (module feature)
  (unless (mem feature module!*features*)
    (push feature module!*features*)))

(def process (inline-tests module out src x)
  (if (caris x 'use)
       (process-use inline-tests module out x)
      (caris x 'provides)
       (add-feature module (cadr x))
       (execf module out src x)))

(= source-dirs '("arcsrc" "arctests" "qq" "qqtests" "src" "xboot"))

(def findsrc (name)
  (aand (findfile rootdir source-dirs (+ (asfilename name) ".arc"))
        (+ rootdir it)))

(def findtest (name)
  (aand (findfile rootdir source-dirs (+ (asfilename name) ".t"))
        (+ rootdir it)))

(def loadfile (inline-tests module out src)
  (prn src)

  (each x (readfile src)
    (process inline-tests module out src x)))

(def runtest (inline-tests module out name)
  (awhen (findtest name)
    (loadfile inline-tests module out it)))

(def xload (inline-tests module out name)
  (when (isa name 'sym)
    (add-feature module name))
  (let src (if (isa name 'sym) (findsrc name) name)
    (unless src
      (err "src not found" name))
    (loadfile inline-tests module out src))
  (when inline-tests
    (runtest inline-tests module out name)))

(let module (create-boot-module)
  (w/outfile out "../xboot/boot-test.expanded"
    (runtest t module out '$quote)
    (runtest t module out '$if)
    (xload t module out 'container)))

(let module (create-boot-module)
  (w/outfile out "../xboot/boot.expanded"
    (xload nil module out 'container)))
