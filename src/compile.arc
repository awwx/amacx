(use arcbase aif quasiquote when)

(def functional-extend (g nk nv)
  (fn (k)
    (if (is k nk) nv (g k))))

(def heuristic-loc (loc e)
  (if (hasloc e) e (srcloc loc e)))

(assign compiler-rules (table))

(if (is runtime 'arc3_2)
  (mac compiler-rule (name test-expr . body)
    `(sref compiler-rules
       (obj test   (fn (context e) ,test-expr)
            action (fn (context e it) ,@body))
       ',name))
  (mac compiler-rule (name test-expr . body)
    `(sref compiler-rules ',name
       (obj test   (fn (context e) ,test-expr)
            action (fn (context e it) ,@body)))))

(def match-compiler-rule (rules context e)
  (when rules
    (let rule (car rules)
      (aif ((rule 'test) context e)
            ((rule 'action) context e it)
            (match-compiler-rule (cdr rules) context e)))))

(def compile (context x)
  (heuristic-loc x
    (or (match-compiler-rule (context 'rules) context x)
        (err "invalid expression" x))))
