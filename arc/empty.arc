(use arcbase type len)

; Arc 3.1 arc.arc:205

(def empty (seq)
  (or (no seq)
      (and (or (is (type seq) 'string) (is (type seq) 'table))
           (is (len seq) 0))))
