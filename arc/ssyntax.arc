(use ac expand-ssyntax)

(ac-rule ssyntax-sym (is-ssyntax e)
  (compile context (expand-ssyntax e)))

(ac-rule ssyntax-form (and (acons e) (is-ssyntax (car e)))
  (compile context
    (cons (expand-ssyntax (car e))
          (cdr e))))

(extend-ac 'ssyntax-sym 'ssyntax-form)
