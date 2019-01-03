(use ac expand-ssyntax)

(compiler-rule ssyntax-sym (is-ssyntax e)
  (compile context (expand-ssyntax e)))

(compiler-rule ssyntax-form (and (acons e) (is-ssyntax (car e)))
  (compile context
    (cons (expand-ssyntax (car e))
          (cdr e))))

(extend-ac 'ssyntax-sym 'ssyntax-form)
