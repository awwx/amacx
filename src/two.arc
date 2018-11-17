(mac w/uniq (names . body)
  (if (acons names)
      `(,with ,(apply join nil (map1 (fn (n) `(,n (,uniq ',n)))
                                 names))
         ,@body)
      `(,let ,names (,uniq ',names) ,@body)))

(mac withs (parms . body)
  (if (no parms)
      `(,do ,@body)
      `(,let ,(car parms) ,(cadr parms)
         (,withs ,(cddr parms) ,@body))))

(test withs)

; Argument destructuring and optional arguments.

(def fn-complex-args? (args)
  (if (no args)
       nil
      (isa args 'sym)
       nil
      (and (acons args) (isa (car args) 'sym))
       (fn-complex-args? (cdr args))
       t))

(def fn-complex-opt (var expr ra)
  `((,var (,if (,acons ,ra)
               (,car ,ra)
               ,expr))))

(def fn-complex-args (args ra)
  (if (no args)
       nil
      (isa args 'sym)
       `((,args ,ra))
      (acons args)
       (let x (if (and (acons (car args)) (is (caar args) 'o))
                   (fn-complex-opt (cadar args)
                                   (if (acons (cddar args)) (caddar args) nil)
                                   ra)
                   (fn-complex-args (car args) `(,car ,ra)))
         (join x (fn-complex-args (cdr args) `(,cdr ,ra))))
       (err "Can't understand fn arg list" args)))

(def fn-complex-fn (args body)
  (w/uniq ra
    (let z (apply join (fn-complex-args args ra))
      `($fn ,ra
         (,withs ,z ,@body)))))

(def fn-body (body)
  (if (no body)
       '(nil)
       body))

(mac fn (args . body)
  (if (fn-complex-args? args)
       (fn-complex-fn args body)
       `($fn ,args ,@(fn-body body))))

(test fn)

(mac aif (expr . body)
  `(,let it ,expr
     (,if it
         ,@(if (cddr body)
               `(,(car body) (,aif ,@(cdr body)))
               body))))

(test aif)

(mac rfn (name parms . body)
  `(,let ,name nil
     (,assign ,name (,fn ,parms ,@body))))

(test rfn)

(mac afn (parms . body)
  `(,let self nil
     (,assign self (,fn ,parms ,@body))))

(test afn)

(def rev (xs)
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(test rev)

(mac accum (accfn . body)
  (w/uniq gacc
    `(,withs (,gacc nil ,accfn (,fn (x) (,assign ,gacc (,cons x ,gacc))))
       ,@body
       (,rev ,gacc))))

(test accum)

(def foreach (seq f)
  (if (alist seq)
       ((afn (seq)
          (when (acons seq)
            (f (car seq))
            (self (cdr seq))))
        seq)

      (isa seq 'table)
       (foreach (keys seq)
         (fn (k)
           (f (list k (seq k)))))

       (for i 0 (- (len seq) 1)
         (f (seq i)))))

(mac each (var expr . body)
  `(,foreach ,expr (,fn (,var) ,@body)))

(def len (xs (o l 0))
  (if (no xs)
       l
      (acons xs)
       (len (cdr xs) (+ l 1))
       (err "invalid list" xs)))

(test len)

(mac when (test . body)
  `(,if ,test (,do ,@body)))

(mac unless (test . body)
  `(,if (,no ,test) (,do ,@body)))

(def listtab (al)
  (let h (table)
    (map1 (fn ((k v)) (sref h k v))
          al)
    h))

(test listtab)

(mac obj args
  `(,listtab (,list ,@(map1 (fn ((k v))
                              `(,list ',k ,v))
                           (pair args)))))

(test obj)

(def amacro (x)
  (and (isa x 'mac) x))

(test amacro)

(def macro (module x)
  (or (amacro x)
      (and (isa x 'sym)
           (has module x)
           (amacro (module x)))))

(test macro)

; as we don't have mem yet

(def contains (lst x)
  (if (no lst)
       nil
      (is (car lst) x)
       t
       (contains (cdr lst) x)))

(def is-lexical (context var)
  (and (isa var 'sym) (contains (context 'env) var)))

(def arglist (args)
  (if (no args)
       nil
      (isa args 'sym)
       (list args)
      (and (cdr args) (isa (cdr args) 'sym))
       (list (car args) (cdr args))
       (cons (car args) (arglist (cdr args)))))

(def extend (g nk nv)
  (fn (k)
    (if (is k nk) nv (g k))))

(def extend-env (context vars)
  (extend context 'env
    (join vars (context 'env))))

(def idfn (x) x)

(def macro-expand (context e)
  ((or (context 'validate) idfn)
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
       `($quote ,(context 'module))
       (let module-var-macro ((context 'module) 'module-var)
         (unless module-var-macro
           (err "no module-var macro defined" var))
         (macro-expand context `(,module-var-macro ,var)))))

(def macro-expand-call (context es)
  `($call ,@(map-macro-expand context es)))

(def macro-expand-fn (context e)
  (let context (extend-env context (arglist (cadr e)))
    `($fn ,(cadr e)
       ,@(map-macro-expand context (cddr e)))))

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
