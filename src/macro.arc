(use arcbase aif quasiquote unless contains $ail)

(def amacro (x)
  (and (isa x 'mac) x))

(def macro (module x)
  (or (amacro x)
      (and (isa x 'sym)
           (has module x)
           (amacro (module x)))))

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

(def heuristic-loc (loc e)
  (if (hasloc e) e (srcloc loc e)))

($ail
  (def macro-expand (context e)
    ((or (context 'validate) idfn)
      (aif (no e)
            (srcloc e `($quote ,e))

           (isa e 'sym)
            (macro-expand-var context e)

           (caris e '$quote)
            (srcloc e `($quote ,(cadr e)))

           (caris e '$assign)
            (macro-expand-assign context (cadr e) (caddr e))

           (caris e '$fn)
            (macro-expand-fn context e)

           (caris e '$if)
            (macro-expand-if context e)

           (caris e '$call)
            (macro-expand-call context e (cdr e))

           (and (acons e)
                (no (is-lexical context (car e)))
                (macro (context 'module) (car e)))
            (macro-expand-macro context e it (cdr e))

           (acons e)
            (macro-expand-call context e e)

            (srcloc e `($quote ,e)))))

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

  (def macro-expand-call (context loc es)
    (srcloc loc
      `($call ,@(map-macro-expand context es))))

  (def macro-expand-fn (context e)
    (let context (extend-env context (arglist (cadr e)))
      (srcloc e
        `($fn ,(cadr e)
           ,@(map-macro-expand context (cddr e))))))

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

  (def macro-expand-macro (context loc macro args)
    (let expansion (heuristic-loc loc (apply (rep macro) args))
      (macro-expand context expansion))))
