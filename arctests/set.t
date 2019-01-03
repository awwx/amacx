(use arc equals)

(let x nil
  (set x)
  (equals x t))

(let g (obj a 1 b 2)
  (set g!foo)
  (equals (g 'foo) t))

(let m (list nil nil nil)
  (set (car m))
  (equals m '(t nil nil)))

(let m (list nil nil nil)
  (set (cadr m))
  (equals m '(nil t nil)))

(let m (list nil nil nil)
  (set (m 2))
  (equals m '(nil nil t)))
