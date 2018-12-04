(use arcbase len all square-fn some iso)

(def sets-iso (s1 s2)
  (and (is (len s1) (len s2))
       (all (fn (element)
              (some [iso _ element] s2))
            s1)))
