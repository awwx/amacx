(use mac uniq simple-fn afn if list apply idfn)

(mac compose args
  (let g (uniq)
    `(,fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(,apply ,(if (car fs) (car fs) idfn) ,g)))
         args))))
