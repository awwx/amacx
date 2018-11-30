(use simple-def aif and isa)

(def replace-tree (x mapping)
  (aif (and (isa x 'sym) (mapping x))
        it
       (acons x)
        (cons (replace-tree (car x) mapping)
              (replace-tree (cdr x) mapping))
        x))
