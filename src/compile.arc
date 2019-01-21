(use arcbase aif quasiquote when complex-fn validate-ail ret)

; TODO check if we do have ssyntax now.
;
; Note this code also runs in Arc 3.2 for bootstrapping.

(def functional-extend (g nk nv)
  (fn (k)
    (if (is k nk) nv (g k))))

(def heuristic-loc (loc e)
  (if (hasloc e) e (srcloc loc e)))

(assign compiler-rules (table))

; The runtime test handles the arguments of `sref` being
; `(sref g value key)` in Arc 3.2 and `(sref g key value)`
; in Amacx.

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

(def compile (context e)
  (ret ail
       (heuristic-loc e
         (or (match-compiler-rule (context 'rules) context e)
             (err "invalid expression" e)))
    (when (context 'validate-ail)
      (validate-ail ail))))

(def gen-compiler (rule-names)
  (let rules (map1 (fn (rule-name)
                     (or (compiler-rules rule-name)
                         (err "compiler rule not found" rule-name)))
                   rule-names)
    (fn (container e (o options))
      (let context (obj rules        rules
                        rule-names   rule-names
                        container    container
                        env          '()
                        validate-ail (and options (options 'validate-ail)))
        (compile context e)))))
