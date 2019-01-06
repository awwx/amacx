(use arcbase testify reclist square-fn compose)

; Arc 3.2 arc.arc:235

(def mem (test seq)
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))
