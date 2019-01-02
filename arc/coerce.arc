(use arcbase when case as-int as-str as-sym as-num as-cons)

; Arc 3.2 ac.scm:954

(def coerce (x type . args)
  (when (a-tagged x)
    (err "Can't coerce annotated object"))
  (apply
    (case type
      int    as-int
      string as-str
      sym    as-sym
      num    as-num
      cons   as-cons
             (err "Can't coerce" x type))
    x
    args))
