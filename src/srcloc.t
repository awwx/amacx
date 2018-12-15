(use arcbase readstr equals case)

(equals (loc (cadr (readstr "(a b c)")))
        (case runtime
          mpair  nil
          srcloc "string:1.3"
          (err "oops" runtime)))
