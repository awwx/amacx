; TODO incomplete

(use arcbase quasiquote complex-fn ssyntax square-fn def assoc withs
     rfn afn compose complement rev isnt w/uniq in iso when unless
     while empty reclist recstring testify some all mem find map
     mappend > warn atomic setforms setform-cons forloop for accum)

; Arc 3.2 arc.arc:282

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

; If ok to do with =, why not with def?  But see if use it.

; TODO test

(mac defs args
  `(do ,@(map [cons 'def _] (tuples args 3))))

; Arc 3.2 arc.arc:458

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(,with (,v nil ,gi ,init ,gm (,- ,min 1))
       (,forloop (,assign ,v ,gi) (,> ,v ,gm) (,assign ,v (- ,v 1))
         ,@body))))
