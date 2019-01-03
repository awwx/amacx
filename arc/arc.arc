; TODO incomplete

(use arcbase +list +str quasiquote complex-fn ssyntax square-fn def
     assoc withs rfn afn compose complement rev isnt w/uniq in iso
     when unless while empty reclist recstring testify some all mem
     find map mappend > warn atomic setforms setform-cons forloop for
     accum repeat each whilet coerce even do1 caselet case pr prn
     tostring keys aif whiler string even after w/open w/outstring
     w/stdout fromstring read readc writec readb)

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

(mac zap (op place . args)
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(,atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

; Arc 3.2 arc.arc:656

(def prt args
  (map1 [if _ (disp _)] args)
  (car args))

; Arc 3.2 arc.arc:664

(mac wipe args
  `(,do ,@(map (fn (a) `(,= ,a nil)) args)))

(mac set args
  `(,do ,@(map (fn (a) `(,= ,a ,t)) args)))

(mac iflet (var expr then . rest)
  (w/uniq gv
    `(,let ,gv ,expr
       (,if ,gv (,let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(,iflet ,var ,expr (,do ,@body)))

; Arc 3.2 arc.arc:687

(mac awhen (expr . body)
  `(,let it ,expr (,if it (,do ,@body))))

(mac aand args
  (if (no args)
      't
      (no (cdr args))
       (car args)
      `(,let it ,(car args) (,and it (,aand ,@(cdr args))))))

(mac drain (expr (o eof nil))
  (w/uniq (gacc gdone gres)
    `(,with (,gacc nil ,gdone nil)
       (,while (,no ,gdone)
         (,let ,gres ,expr
           (,if (,is ,gres ,eof)
               (,= ,gdone t)
               (,push ,gres ,gacc))))
       (,rev ,gacc))))

; Arc 3.2 arc.arc:732

(def consif (x y) (if x (cons x y) y))

; Arc 3.2 arc.arc:737

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (w/uniq gx
    `(,let ,gx ,x
       (,if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)
                nil
               (f (car seq))
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq)
         start)
        (recstring [if (f (seq _)) _] seq start))))

; Arc 3.2 arc.arc:764

(def odd (n) (no (even n)))

; Arc 3.2 arc.arc:796

(mac w/appendfile (var name . body)
  `(,let ,var (,outfile ,name 'append)
     (,after (,do ,@body) (,close ,var))))

; Arc 3.2 arc.arc:816

(def readstring1 (s (o eof nil)) (w/instring i s (read i eof)))

; Arc 3.2 arc.arc:823

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name) (w/infile s name (read s)))

(def readall (src (o eof nil))
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(def sym (x) (coerce x 'sym))

(def int (x (o b 10)) (coerce x 'int b))

(mac rand-choice exprs
  `(,case (,rand ,(len exprs))
     ,@(let key -1
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  (w/uniq ga
    `(,let ,ga nil
       (,repeat ,n (,push ,expr ,ga))
       (,rev ,ga))))

(def rand-string (n)
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0)
      (w/infile str "/dev/urandom"
        (while (< i n)
          (let x (readb str)
             (unless (> x 247)
               (= (s i) (c (mod x nc)))
               (++ i)))))
      s)))

(mac forlen (var s . body)
  `(,for ,var 0 (,- (,len ,s) 1) ,@body))
