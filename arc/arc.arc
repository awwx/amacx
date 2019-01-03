; TODO incomplete

(use arcbase +list +str quasiquote complex-fn ssyntax square-fn def
     assoc withs rfn afn compose complement rev isnt w/uniq in iso
     when unless while empty reclist recstring testify some all mem
     find map mappend > warn atomic setforms setform-cons forloop for
     accum repeat each whilet coerce even do1 caselet case)

(def copylist (xs)
  (apply1 list xs))

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

; Arc 3.2 arc.arc:484

; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
  (let end (if (no end)   (len seq)
               (< end 0)  (+ (len seq) end)
                          end)
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))

; Arc 3.2 arc.arc:504

(def last (xs)
  (if (cdr xs)
      (last (cdr xs))
      (car xs)))

(def rem (test seq)
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

; Arc 3.2 arc.arc:524

(def keep (test seq)
  (rem (complement (testify test)) seq))

(def trues (f xs)
  (and xs
      (let fx (f (car xs))
        (if fx
            (cons fx (trues f (cdr xs)))
            (trues f (cdr xs))))))

; Arc 3.2 arc.arc:558

(mac push (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(,let ,gx ,x
         (,atwiths ,binds
           (,setter (,cons ,gx ,val)))))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(,atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(,atwiths ,(mappend (fn (g (binds val setter))
                           (+ binds (list g val)))
                         vars
                         forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(,atwiths ,(+ binds (list g val))
         (,do1 (,car ,g)
               (,setter (,cdr ,g)))))))

(def adjoin (x xs (o test iso))
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(,atwiths ,(+ (list gx x) binds)
         (,setter (,adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(,atwiths ,(+ (list g test) binds)
         (,setter (,rem ,g ,val))))))

(mac togglemem (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(,atwiths ,(+ (list gx x) binds)
         (,setter (,if (,mem ,gx ,val)
                       (,rem ,gx ,val)
                       (,adjoin ,gx ,val ,@args)))))))

(mac ++ (place (o i 1))
  (if (isa place 'sym)
      `(,= ,place (,+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(,withs ,(+ binds (list gi i))
             (,setter (,+ ,val ,gi)))))))

(mac -- (place (o i 1))
  (if (isa place 'sym)
      `(,= ,place (,- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(,withs ,(+ binds (list gi i))
             (,setter (,- ,val ,gi)))))))
