(set include-tests)

(mac test body
  (if include-tests `(do ,@body)))

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

(mac as (type expr)
  `(coerce ,expr ',type))

(test
  (equals (as sym "foo") 'foo))

(def replace-tree (x mapping)
  (aif (and (isa x 'sym) (mapping x))
        it
       (acons x)
        (cons (replace-tree (car x) mapping)
              (replace-tree (cdr x) mapping))
        x))

(def renaming-table (renames)
  (ret tab (table)
    (each (k v) (pair renames)
      (= (tab k) v))))

(= $renames
   (renaming-table
      '($quote   quote-xVrP8JItk2Ot
        $fn      fn-xVrP8JItk2Ot
        $assign  assign-xVrP8JItk2Ot
        $if      if-xVrP8JItk2Ot
        $call    call-xVrP8JItk2Ot)))

(mac $renaming body
  `(do ,@(replace-tree body $renames)))

($renaming
  (def ail-quote? (x)
    (and (caris x '$quote)
         (is (len x) 2)))

  (def ail-assign? (x)
    (and (caris x '$assign)
         (is (len x) 3)
         (isa (x 1) 'sym)
         (ail-expr? (x 2))))

  (def ail-fn? (x)
    (and (caris x '$fn)
         (>= (len x) 3)
         ; todo check argument list
         (all ail-expr? (cddr x))))

  (def ail-if? (x)
    (and (caris x '$if)
         (is (len x) 4)
         (all ail-expr? (cdr x))))

  (def ail-call? (x)
    (and (caris x '$call)
         (all ail-expr? (cdr x))))

  (def ail-expr? (x)
    (or (isa x 'sym)
        (ail-quote? x)
        (ail-assign? x)
        (ail-fn? x)
        (ail-if? x)
        (ail-call? x)))

  (def validate-ail (x)
    (unless (ail-expr? x)
      (err "not a valid ail language expr" x))
    x)

  (test
    (equals (ail-expr? '($fn (x)
                          ($call ($quote +) x ($quote 10))))
            t))

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
            '(a b c)))

  (def amacro (x)
    (and (isa x 'mac) x))

  (def macro (module x)
    (or (amacro x)
        (and (isa x 'sym)
             (has module x)
             (amacro (module x)))))

  (def contains (lst x)
    (if (no lst)
         nil
        (is (car lst) x)
         t
         (contains (cdr lst) x)))

  (def is-lexical (context var)
    (and (isa var 'sym) (contains (context 'env) var)))

  (def macro-expand (context e)
    (validate-ail
      (aif (no e)
            `($quote nil)

           (isa e 'sym)
            (macro-expand-var context e)

           (caris e '$quote)
            `($quote ,(cadr e))

           (caris e '$assign)
            (macro-expand-assign context (cadr e) (caddr e))

           (caris e '$fn)
            (macro-expand-fn context e)

           (caris e '$if)
            (macro-expand-if context e)

           (caris e '$call)
            (macro-expand-call context (cdr e))

           (and (acons e)
                (no (is-lexical context (car e)))
                (macro (context 'module) (car e)))
            (macro-expand-macro context it (cdr e))

           (acons e)
            (macro-expand-call context e)

            `($quote ,e))))

  (test
    (equals (macro-expand (obj) '($quote 123))
            '($quote 123)))

  (def map-macro-expand (context es)
    (map1 (fn (e)
            (macro-expand context e))
          es))

  (def macro-expand-var (context var)
    (if (is-lexical context var)
         var
         (macro-expand-module-var context var)))

  (def macro-expand-module-var (context var)
    (if (is var '*module*)
         `($quote ,context!module)
         (let module-var-macro (context!module 'module-var)
           (unless module-var-macro
             (err "no `module-var` macro defined" var))
           (macro-expand context `(,module-var-macro ,var)))))

  (test
    (equals (macro-expand (obj module 'my-module) '*module*)
            '($quote my-module)))

  (def macro-expand-call (context es)
    `($call ,@(map-macro-expand context es)))

  (test
    (= zilch (obj module (obj)))

    (equals (macro-expand zilch '(($quote a) ($quote 42)))
            '($call ($quote a) ($quote 42))))

  (def arglist (args)
    (if (no args)
         nil
        (isa args 'sym)
         (list args)
        (and (cdr args) (isa (cdr args) 'sym))
         (list (car args) (cdr args))
         (cons (car args) (arglist (cdr args)))))

  (def extend (table nk nv)
    (fn (k)
      (if (is k nk) nv (table k))))

  (def extend-env (context vars)
    (extend context 'env
      (join vars context!env)))

  (def macro-expand-fn (context e)
    (let context (extend-env context (arglist (cadr e)))
      `($fn ,(cadr e)
         ,@(map-macro-expand context (cddr e)))))

  (test
    (equals (macro-expand zilch '($fn (a) a))
            '($fn (a) a)))

  (def macro-expand-if (context e)
    `($if ,@(map-macro-expand context (cdr e))))

  (def macro-expand-assign (context var val)
    (unless var
      (err "assign: variable not specified"))

    (unless (isa var 'sym)
      (err "assign: not a sym" var))

    (if (is-lexical context var)
         `($assign ,var ,(macro-expand context val))
         (macro-expand-assign-module-var context var val)))

  (def macro-expand-assign-module-var (context var val)
    (let set-module-var ((context 'module) 'set-module-var)
      (unless set-module-var
        (err "set-module-var macro not defined" var))
      (macro-expand context `(,set-module-var ,var ,val))))

  (def macro-expand-macro (context macro args)
    (let expansion (apply (rep macro) args)
      (macro-expand context expansion)))

  (def expand-eval-arc (context x)
    (eval:ailarc:macro-expand context x))

  (test
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

(= boot-module
   (obj acons         [isa _ 'cons]
        annotate      annotate
        apply         apply
        ar-apply      apply
        ar-assert     ar-assert
        ar-disp       disp
        ar-iso        iso
        ar-is2        is
        ar-strlen     len
        ar-str-append (fn args (apply + "" args))
        ar-symstr     [coerce _ 'string]
        ar-tag-type   type
        ar-uniq       ar-uniq
        ar-write      write
        ar-<2         <
        a-char        [isa _ 'char]
        a-fn          [isa _ 'fn]
        a-num         [isa _ 'num]
        a-str         [isa _ 'string]
        a-sym         [isa _ 'sym]
        a-table       [isa _ 'table]
        a-tagged      a-tagged
        an-input      [isa _ 'input]
        an-int        [isa _ 'int]
        an-output     [isa _ 'output]
        car           car
        cdr           cdr
        cons          cons
        err           err
        fnname        fnname
        has           has
        namefn        namefn
        rep           rep
        sref          (fn (g k v) (sref g v k))
        stderr        stderr
        stdin         stdin
        stdout        stdout
        t             t
        table         table
        +             +
        -             -
        *             *
        /             /))

(= boot-context (obj module boot-module))

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

(test
  (equals (tostring (write (munch boot-module `(a ,boot-module b))))
          "(a #&*module* b)")

  (equals (tostring (write (munch boot-module `(a ,boot-module!cons b))))
          "(a #&cons b)"))

(= out (outfile "boot.expanded"))

(def load-test (testname)
  (readfile (+ "../tests/" testname ".t")))

(def include (x)
  (if (caris x 'test)
       (if include-tests (load-test (cadr x)))
       (list x)))

(def execf (out x)
  (let x (replace-tree x $renames)
    ; (pprint x)
    (let m (macro-expand boot-context x)
      (when out
        (write (munch boot-module m) out)
        (disp "\n\n" out))
      (eval (ailarc m)))))

(def xload (out filename)
  (each x (readfile filename)
    (each y (include x)
      (execf out y))))

(xload out "../src/boot.arc")
(xload out "../qq/qq.arc")

(when include-tests
  (each filename '("proper.t" "dotted-list.t" "qq.t")
    (xload out (+ "../tests/" filename))))

(xload out "../src/two.arc")

(close out)
