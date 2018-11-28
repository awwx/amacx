(use rule macro expand-ssyntax)

(rule macro-expand (context e) (is-ssyntax e)
  (macro-expand context (expand-ssyntax e)))

(rule macro-expand (context e) (and (acons e) (is-ssyntax (car e)))
  (macro-expand context (cons (expand-ssyntax (car e)) (cdr e))))
