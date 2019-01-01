(use arcbase)

; Arc 3.2 arc.arc:92

(def assoc (key al)
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

; Arc 3.2 arc.arc:99

(def alref (al key) (cadr (assoc key al)))
