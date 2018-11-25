(use def if alist afn when isa simple-fn list for)

(def foreach (seq f)
  (if (alist seq)
       ((afn (seq)
          (when (acons seq)
            (f (car seq))
            (self (cdr seq))))
        seq)

      (isa seq 'table)
       (table-each seq (fn (k v)
                         (f (list k v))))

       (for i 0 (- (len seq) 1)
         (f (seq i)))))
