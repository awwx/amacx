(use arcbase aif quasiquote unless contains $ail when obj validate-ail)

(def amacro (x)
  (and (isa x 'mac) x))

(def macro (container x)
  (or (amacro x)
      (and (isa x 'sym) (amacro (container x nil)))))

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

(assign compiler-rules (table))

(if (is runtime 'arc3_2)
  (mac ac-rule (name test-expr . body)
    `(sref compiler-rules
       (obj test   (fn (context e) ,test-expr)
            action (fn (context e it) ,@body))
       ',name))
  (mac ac-rule (name test-expr . body)
    `(sref compiler-rules ',name
       (obj test   (fn (context e) ,test-expr)
            action (fn (context e it) ,@body)))))

(def module-var-macro (context)
  (let macro ((context 'container) 'module-var nil)
    (unless macro
      (err "need module-var macro defined for topvar"))
    macro))

(def set-topvar-macro (context)
  (let macro ((context 'container) 'set-module-var nil)
    (unless macro
      (err "need set-module-var macro defined to assign to a topvar" var))
    macro))

(def check-assign (var)
  (unless var
    (err "assign: variable not specified"))

  (unless (isa var 'sym)
    (err "assign: not a sym" var)))

(def macro-form (context e)
  (and (acons e)
       (no (is-lexical context (car e)))
       (macro (context 'container) (car e))))

(def compile (context x)
  (heuristic-loc x
    (or (match-rule (context 'rules) context x)
        (err "invalid expression" x))))

(def map-compile (context xs)
  (map1 (fn (x)
          (compile context x))
        xs))

($ail
  (def compile-call (context loc es)
    `($call ,@(map-compile context es)))

  (ac-rule nil-sym (no e)
    `($quote ,e))

  (ac-rule lexvar (and (isa e 'sym) (is-lexical context e))
    e)

  (ac-rule this-container (is e 'this-container)
    `($quote ,(context 'container)))

  (ac-rule topvar (isa e 'sym)
    (compile context `(,(module-var-macro context) ,e)))

  (ac-rule quote (caris e '$quote)
    `($quote ,(cadr e)))

  (ac-rule assign-lexvar (and (caris e '$assign)
                              (is-lexical context (cadr e)))
    (check-assign (cadr e))
    `($assign ,(cadr e) ,(compile context (caddr e))))

  (ac-rule assign-topvar (caris e '$assign)
    (check-assign (cadr e))
    (compile context
      `(,(set-topvar-macro context) ,(cadr e) ,(caddr e))))

  (ac-rule fn (caris e '$fn)
    (let context (extend-env context (arglist (cadr e)))
      `($fn ,(cadr e)
         ,@(map-compile context (cddr e)))))

  (ac-rule if (caris e '$if)
    `($if ,@(map-compile context (cdr e))))

  (ac-rule explicit-call (caris e '$call)
    (compile-call context e (cdr e)))

  (ac-rule macro (macro-form context e)
    (let expansion (apply (rep it) (cdr e))
      (compile context expansion)))

  (ac-rule implicit-call (acons e)
    (compile-call context e e))

  (ac-rule default-quote t
    `($quote ,e)))

(assign ac-rules
  '(nil-sym this-container lexvar topvar quote assign-lexvar
    assign-topvar fn if explicit-call macro implicit-call
    default-quote))

(def gen-ac (rule-names)
  (let rules (map1 (fn (rule-name)
                     (or (compiler-rules rule-name)
                         (err "compiler rule not found" rule-name)))
                   rule-names)
    (fn (container e)
      (let context (obj rules      rules
                        rule-names rule-names
                        container  container
                        env        '())
        (compile context e)))))

(def match-rule (rules context e)
  (when rules
    (let rule (car rules)
      (aif ((rule 'test) context e)
            ((rule 'action) context e it)
            (match-rule (cdr rules) context e)))))

(assign compile-xVrP8JItk2Ot (gen-ac ac-rules))

(def extend-ac rule-names
  (assign ac-rules (join rule-names ac-rules))
  (assign compile-xVrP8JItk2Ot (gen-ac ac-rules)))
