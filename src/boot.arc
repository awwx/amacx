(test ail)

; Bootstrap!
;
; (mac module-var (var)
;   `(,*module* ($quote ,var)))
;
; -->
;
; (sref *module* 'module-var
;   (annotate 'mac
;     (fn (var)
;       (cons *module*
;         (cons (cons '$quote (cons var nil))
;           nil)))))
;
; -->

((*module* ($quote sref))
  *module*
  ($quote module-var)
  ((*module* ($quote annotate))
    ($quote mac)
    ($fn (var)
      ((*module* ($quote cons))
       *module*
       ((*module* ($quote cons))
        ((*module* ($quote cons))
         ($quote $quote)
         ((*module* ($quote cons))
          var
          ($quote nil)))
        ($quote nil))))))

(test module-var)

; (mac set-module-var (var value)
;   `(,sref ,*module* ',var ,value))

((*module* ($quote sref))
  *module*
  ($quote set-module-var)
  (annotate ($quote mac)
    ($fn (var value)
      (cons sref
        (cons *module*
          (cons (cons ($quote $quote)
                      (cons var nil))
            (cons value nil)))))))

; (mac quote (x)
;   `($quote ,x))

($assign quote
  (annotate ($quote mac)
    ($fn (x)
      (cons ($quote $quote) (cons x nil)))))

; (mac assign args
;   `($assign ,@args))

($assign assign
  (annotate 'mac
    ($fn args
      (cons '$assign args))))

(test assign)

; (mac fn (parms . body)
;   `($fn ,parms ,@body))

(assign fn
  (annotate 'mac
    ($fn (parms . body)
      (cons '$fn (cons parms body)))))

(test simple-fn)

; (mac named-fn (name parms . body)
;   `(,namefn ',name (,fn ,parms ,@body)))

(assign named-fn
  (annotate 'mac
    (fn (name parms . body)
      (cons namefn
        (cons (cons quote (cons name nil))
          (cons (cons fn (cons parms body))
                nil))))))

(test named-fn)

; (def list args args)

(assign list (named-fn list args args))

(test list)

; (assign mac
;   (annotate 'mac
;     (named-fn mac (name parms . body)
;       `(,assign ,name (,annotate 'mac (,named-fn ,name ,parms ,@body))))))

(assign mac
  (annotate 'mac
    (named-fn mac (name parms . body)
      (list assign name
            (list annotate ''mac
                  (cons named-fn (cons name (cons parms body))))))))

; (mac if args
;   `($if ,@args))

(mac if args
  (cons '$if args))

(test simple-if)

; (mac do body
;   `(($fn () ,@body))

(mac do body
  (if (ar-is2 body nil)
       nil
       (list (cons '$fn (cons '() body)))))

(test simple-do)

; (mac def (name parms . body)
;   `(,assign ,name (,named-fn ,name ,parms ,@body)))

(mac def (name parms . body)
  (cons assign
    (cons name
      (cons (cons named-fn (cons name (cons parms body)))
            nil))))

(def is (a b)
  (ar-is2 a b))

(test acons)

(def caar   (xs) (car (car xs)))
(def cadr   (xs) (car (cdr xs)))
(def cdar   (xs) (cdr (car xs)))
(def cddr   (xs) (cdr (cdr xs)))
(def cadar  (xs) (car (cdar xs)))
(def caddr  (xs) (car (cddr xs)))
(def cddar  (xs) (cdr (cdar xs)))
(def cdddr  (xs) (cdr (cddr xs)))
(def caddar (xs) (car (cddar xs)))
(def cadddr (xs) (car (cdddr xs)))

(test cxr)

(def no (x)
  (is x nil))

(def atom (x)
  (no (acons x)))

(def isnt (x y) (no (is x y)))

; (mac if args
;   (if (no args)
;        nil
;       (no cdr args)
;        (car args)
;        `(,if ,(car args)
;               ,(cadr args)
;               (,if ,@(cddr args)))))

(mac if args
  ($if (no args)
    nil
    ($if (no (cdr args))
      (car args)
      (list '$if (car args)
                  (cadr args)
                  (cons if (cddr args))))))

(test multi-clause-if)

(def map1 (f xs)
  (if (no xs)
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))

; (def pair (xs (o f list)) ...)

(def pair (xs . rs)
  ((fn (f)
     (if (no xs)
          nil
          (if (no (cdr xs))
               (list (list (car xs)))
               (cons (f (car xs) (cadr xs))
                     (pair (cddr xs) f)))))
   (if (is rs nil) list (car rs))))

(test pair)

(mac assert (x)
  (list if x
         (list do (list ar-disp "OK " '(stdout))
                  (list ar-write (list quote x) '(stdout))
                  (list ar-disp #\newline '(stdout)))
         (list 'err "fail" (list quote x))))

(test assert)

; (mac with (parms . body)
;   `((,fn ,(map1 car (pair parms))
;      ,@body)
;     ,@(map1 cadr (pair parms))))

(mac with (parms . body)
  (cons
   (cons fn (cons (map1 car (pair parms)) body))
   (map1 cadr (pair parms))))

(test with)

; (mac let (var val . body)
;   `(,with (,var ,val) ,@body))

(mac let (var val . body)
  (cons with (cons (list var val) body)))

(test let)

(def join args
  (if (no args)
       nil
      (let a (car args)
        (if (no (cdr args))
             a
            (no a)
             (ar-apply join (cdr args))
             (cons (car a) (ar-apply join (cons (cdr a) (cdr args))))))))

(test join)

; (mac equals (a b)
;   `(,assert (,ar-iso ,a ,b)))

(mac equals (a b)
  (list assert (list ar-iso a b)))

(test equals)

; (mac and args
;   (if args
;       (if (cdr args)
;           `(,if ,(car args) (,and ,@(cdr args)))
;           (car args))
;       t))

(mac and args
  (if args
      (if (cdr args)
           (list if (car args) (cons and (cdr args)))
           (car args))
      t))

(test and)
(test ar-strlen)

(def uniq args
  (if (no args)
       (ar-uniq nil nil)
       (ar-uniq nil (car args))))

(test uniq)

; (mac or args
;   (and args
;        (w/uniq g
;          `(let ,g ,(car args)
;             (if ,g ,g (or ,@(cdr args)))))))

(mac or args
  (and args
       (let g (uniq)
         (list let g (car args)
            (list if g g (cons or (cdr args)))))))

(test or)

(def iso (x y)
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

(test iso)

; redefine using iso

(mac equals (a b)
  (list assert (list iso a b)))

(test plus)
(test closures)
(test minus)
(test times)
(test annotate)
(test ar-str-append)

(def type (x)
  (if (a-tagged x)     (ar-tag-type x)
      (acons x)        'cons
      (a-sym x)        'sym
      (a-fn x)         'fn
      (a-char x)       'char
      (a-str x)        'string
      (an-int x)       'int
      (a-num x)        'num
      (a-table x)      'table
      (an-output x)    'output
      (an-input x)     'input
      (a-socket x)     'socket
      (an-exception x) 'exception
      (a-thread x)     'thread
      'unknown))

(test type)

#;(def combine-apply (args)
  (if (no args)
       nil
      (no (cdr args))
       (car args)
       (cons (car args) (combine-apply (cdr args)))))

#;(test combine-apply)

#;(def apply (f . args)
  (ar-apply f (combine-apply args)))

#;(test apply)

(def caris (x val)
  (and (acons x)
       (is (car x) val)))

(test caris)

(def alist (x)
  (or (no x) (acons x)))

(test alist)

(def single (x)
  (and (acons x)
       (no (cdr x))))

(test single)

(def dotted (x)
  (if (atom x)
       nil
       (and (cdr x) (or (atom (cdr x))
                        (dotted (cdr x))))))

(test dotted)

(def rreduce (f xs)
  (if (cddr xs)
       (f (car xs) (rreduce f (cdr xs)))
       (apply f xs)))

(test rreduce)

(def isa (x y)
  (is (type x) y))

(def literal (x)
  (if (a-sym x)
       (no x)
      (acons x)
       (or (caris x quote)
           (caris x 'quote)
           (caris x '$quote))
       t))

(test literal)

(def literal-value (x)
  (if (or (isa x 'sym) (isa x 'cons))
       (if (is x 'nil)
            nil
           (or (caris x quote) (caris x 'quote) (caris x '$quote))
            (cadr x)
           (err "not a literal value" x))
       x))

(test literal-value)
