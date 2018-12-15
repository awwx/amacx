(use arcbase accum after equals)

; no setforms yet

(equals (let x 1
          (after (assign x 2)
                 (assign x 3))
          x)
        3)

; Normal return from both before and after clauses.
;
; The value returned from the before clause is returned, and the after
; clause is executed, but its return value is ignored.

(equals
  (accum a
    (a
      (after (do (a 1) 'foo)
        (a 2)
        'ignored)))
  '(1 2 foo))

; TODO more with catch
