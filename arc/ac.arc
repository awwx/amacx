(use arcbase quasiquote unless contains $ail obj compile expand-ssyntax)

(def amacro (x)
  (and (isa x 'mac) x))

(def macro (container x)
  (or (amacro x)
      (and (isa x 'sym) (amacro (container x nil)))))

; Use `contains` here instead of `mem` because we don’t have `mem` yet
; in the load process.

(def is-lexical (context var)
  (and (isa var 'sym) (contains (context 'env) var)))

; Arc 3.2 ac.scm:355

(def arglist (args)
  (if (no args)
       nil
      (isa args 'sym)
       (list args)
      (and (cdr args) (isa (cdr args) 'sym))
       (list (car args) (cdr args))
       (cons (car args) (arglist (cdr args)))))

(def extend-env (context vars)
  (functional-extend context 'env
    (join vars (context 'env))))

(def topvar-macro (context var)
  (let macro ((context 'container) 'topvar nil)
    (unless macro
      (err "need topvar macro defined for top level variable" var))
    macro))

(def set-topvar-macro (context var)
  (let macro ((context 'container) 'set-topvar nil)
    (unless macro
      (err "need set-topvar macro defined to assign to top level variable"
           var))
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

(def map-compile (context xs)
  (map1 (fn (x)
          (compile context x))
        xs))

($ail
  (def compile-call (context loc es)
    `($call ,@(map-compile context es)))

  (compiler-rule nil-sym (no e)
    `($quote ,e))

  (compiler-rule ssyntax-sym (is-ssyntax e)
    (compile context (expand-ssyntax e)))

  (compiler-rule ssyntax-form (and (acons e) (is-ssyntax (car e)))
    (compile context
      (cons (expand-ssyntax (car e))
            (cdr e))))

  (compiler-rule lexvar (and (isa e 'sym) (is-lexical context e))
    e)

  (compiler-rule this-container (is e 'this-container)
    `($quote ,(context 'container)))

  (compiler-rule topvar (isa e 'sym)
    (compile context `(,(topvar-macro context e) ,e)))

  (compiler-rule ail-topvar (caris e '$topvar)
    `($topvar ,(context 'container) ,(cadr e)))

  (compiler-rule quote (caris e '$quote)
    `($quote ,(cadr e)))

  (compiler-rule assign-lexvar (and (caris e '$assign)
                                    (is-lexical context (cadr e)))
    (check-assign (cadr e))
    `($assign ,(cadr e) ,(compile context (caddr e))))

  (compiler-rule assign-topvar (caris e '$assign)
    (check-assign (cadr e))
    (compile context
      `(,(set-topvar-macro context (cadr e)) ,(cadr e) ,(caddr e))))

  (compiler-rule fn (caris e '$fn)
    (let context (extend-env context (arglist (cadr e)))
      `($fn ,(cadr e)
         ,@(map-compile context (cddr e)))))

  (compiler-rule if (caris e '$if)
    `($if ,@(map-compile context (cdr e))))

  (compiler-rule explicit-call (caris e '$call)
    (compile-call context e (cdr e)))

  (compiler-rule macro (macro-form context e)
    (let expansion (apply (rep it) (cdr e))
      (compile context expansion)))

  (compiler-rule implicit-call (acons e)
    (compile-call context e e))

  (compiler-rule default-quote t
    `($quote ,e)))

(assign ac-rules
  '(nil-sym ssyntax-sym ssyntax-form this-container lexvar topvar
    ail-topvar quote assign-lexvar assign-topvar fn if explicit-call
    macro implicit-call default-quote))
