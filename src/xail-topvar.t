(use arcbase xail-topvar equals eval load)

(let container (ail-topvar-container)
  (equals (eval '(+ 1 2) container)
          3)

  (use-features container '(reduce))

  (equals (eval '(reduce + '(1 2 3 4)) container)
          10)

  (load "arctests/len.t" container))

; (let container (ail-topvar-container)
;   (load "perftest.arc" container))
