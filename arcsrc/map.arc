(use simple-def if no map1 afn some apply)

(def map (f . seqs)
  (if (no (cdr seqs))
       (map1 f (car seqs))
       ((afn (seqs)
          (if (some no seqs)
               nil
               (cons (apply f (map1 car seqs))
                     (self (map1 cdr seqs)))))
        seqs)))
