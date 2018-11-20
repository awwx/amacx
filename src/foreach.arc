(use def if alist afn when isa keys fn list for)

(def foreach (seq f)
  (if (alist seq)
       ((afn (seq)
          (when (acons seq)
            (f (car seq))
            (self (cdr seq))))
        seq)

      (isa seq 'table)
       (foreach (keys seq)
         (fn (k)
           (f (list k (seq k)))))

       (for i 0 (- (len seq) 1)
         (f (seq i)))))
