(use arcbase len >= all unless $ail)

($ail
  (def ail-quote (x)
    (and (caris x '$quote)
         (is (len x) 2)))

  (def ail-assign (x)
    (and (caris x '$assign)
         (is (len x) 3)
         (isa (x 1) 'sym)
         (ail-expr (x 2))))

  (def ail-fn (x)
    (and (caris x '$fn)
         (>= (len x) 3)
         ; todo check argument list
         (all ail-expr (cddr x))))

  (def ail-if (x)
    (and (caris x '$if)
         (is (len x) 4)
         (all ail-expr (cdr x))))

  (def ail-call (x)
    (and (caris x '$call)
         (all ail-expr (cdr x))))

  (def ail-topvar (x)
    (and (caris x '$topvar)
         ; TODO cadr is table or namespace
         (a-sym (caddr x))))

  (def ail-expr (x)
    (or (isa x 'sym)
        (ail-quote x)
        (ail-assign x)
        (ail-fn x)
        (ail-if x)
        (ail-call x)
        (ail-topvar x))))

(def validate-ail (x)
  (unless (ail-expr x)
    (err "not a valid ail language expr" x))
  x)
